package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Check finite descent and branch isolation through ordinary subtyping rules. */
class ProvenanceDescentTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Bound diagnostics independently of the default abstract-cut policy. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()
    config.searchSteps = Some(5000)

  /** Restore shared configuration after each diagnostic. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** Two disjoint possible terminals of a finite field chain. */
  private def classes: SubContext =
    SubContext.empty.declClass("Leaf", None).declClass("Wrong", None)

  /** Construct a finite literal field chain without inference or variable unfolding. */
  private def chain(depth: Int, terminal: Type): Type =
    (0 until depth).foldLeft(terminal)((body, _) => TField("next", body))

  /** The existing constrained encoding of Leaf or a field containing another chain. */
  private def carrier(terminal: Type): Type =
    val x = TypeVar("X")
    TUniv(x, TConstrained(TVar(x), Constraint(TVar(x), Direction.Super,
      TJointType(JointMode.Union, terminal, TField("next", TVar(x))))))

  test("finite field descent succeeds while malformed terminals genuinely fail"):
    val leaf = TClass(ClassVar("Leaf"))
    val wrong = TClass(ClassVar("Wrong"))
    subtype(chain(16, leaf), carrier(leaf))(using classes)
    intercept[TypeError](subtype(chain(16, wrong), carrier(leaf))(using classes))

  test("a sibling branch does not inherit its predecessor's smaller representative"):
    val leaf = TClass(ClassVar("Leaf"))
    val source = TJointType(JointMode.Union, chain(12, leaf), chain(14, leaf))
    val session = SearchSession()
    val ctx = classes.mapTrail(_.copy(search = Some(session)))
    subtype(source, carrier(leaf))(using ctx)
    assert(session.abstractRepeats > 0)
    assert(session.cuts == 0)
    assert(ctx.trail.provenanceKeys.isEmpty)

  test("lookup cannot reset the latest representative to an earlier ancestor's other subtree"):
    val leaf = TClass(ClassVar("Leaf"))
    val f = TypeVar("F")
    val other = chain(12, leaf)
    val first = chain(12, TVar(f))
    val source = chain(12, TJointType(JointMode.Union, first, other))
    val session = SearchSession()
    val ctx = classes.declTypeVar(f, TypeVarKind.Rigid)
      .extend(Bound(f, Direction.Sub, other, BoundKind.Asserted))
      .mapTrail(_.copy(search = Some(session)))
    // The judgment has a finite derivation, but F's bound is not a subterm of the most recent
    // first-branch representative. Being a subterm of the original source is insufficient.
    val error = intercept[SearchInconclusive](subtype(source, carrier(leaf))(using ctx))
    assert(!error.global)
    assert(error.reason.contains("provenance"))
    assert(session.cuts > 0)

  test("syntax metadata cannot reuse a proof for a different carrier bound"):
    val leaf = TClass(ClassVar("Leaf"))
    val wrong = TClass(ClassVar("Wrong"))
    val source = chain(12, leaf)
    subtype(source, carrier(leaf))(using classes)
    intercept[TypeError](subtype(source, carrier(wrong))(using classes))

  /** Retain sharing inside each allocation while copying the complete DAG independently. */
  private def doubled(depth: Int): Type =
    (0 until depth).foldLeft[Type](TClass(ClassVar("Leaf")))((body, _) => TLam(body, body))

  test("representative map replacement does not compare deep stored Type values"):
    val previous = doubled(8192)
    val current = doubled(8192)
    val key = ProvenanceKey(Vector(Vector(ProvenanceToken.Label("replacement"))))
    for entries <- List(1, 5) do
      val fillers = (1 until entries).map: i =>
        ProvenanceKey(Vector(Vector(ProvenanceToken.Label(s"unrelated-$i")))) -> (TBot, TTop)
      val trail = SubtypingTrail(provenanceKeys = fillers.toMap.updated(key, (previous, TTop)))
      val updated = trail.copy(provenanceKeys = trail.provenanceKeys.updated(key, (current, TTop)))
      assert(updated.provenanceKeys(key)._1 eq current)
      assert(trail.provenanceKeys(key)._1 eq previous)

  test("a collision updates stored copied DAGs in both small and hash-based path maps"):
    /** Keep a large identical parameter before each finite decreasing return position. */
    def returns(depth: Int, parameter: Type): Type =
      (0 until depth).foldLeft[Type](TClass(ClassVar("Leaf")))((body, _) => TLam(parameter, body))
    val targetVar = TypeVar("Target")
    val target = TVar(targetVar)
    val previous = returns(10, doubled(12))
    val current = returns(9, doubled(12))
    val base = classes.declTypeVar(targetVar, TypeVarKind.Rigid)
      .extend(Bound(targetVar, Direction.Super, TLam(TBot, TTop), BoundKind.Asserted))
    val roots = Set(targetVar)
    val key = ProvenanceKey(current, target, roots)(using base)
    assert(key == ProvenanceKey(previous, target, roots)(using base))
    for entries <- List(1, 5) do
      val fillers = (1 until entries).map: i =>
        ProvenanceKey(Vector(Vector(ProvenanceToken.Label(s"unrelated-$i")))) -> (TBot, TTop)
      val stored = fillers.toMap.updated(key, (previous, target))
      val session = SearchSession()
      val ctx = base.mapTrail(_.copy(search = Some(session), roots = Some(roots),
        provenanceKeys = stored))
      subtype(current, target)(using ctx)
      assert(session.abstractRepeats > 0)
      assert(session.cuts == 0)
