package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.substitute.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Invariants of provenance erasure, search abstraction, and inconclusive-result handling. */
class ProvenanceTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Keep configuration and generated names local to each test. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()

  /** Do not leave a nondefault solver policy on the test runner's worker thread. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** A binder with an explicit source identity, independent of its display name. */
  private def binder(name: String, id: Int): TypeVar =
    new TypeVar(name, VarProvenance(VarOrigin.Source(id), VarRole.Root, Set.empty))

  /** A small class context used by both successful and failing proof queries. */
  private def classes: SubContext =
    SubContext.empty.declClass("Int", None).declClass("Str", None).declClass("Bool", None)

  /** The integer class used in test judgments. */
  private def int: Type = TClass(ClassVar("Int"))

  /** The string class used in test judgments. */
  private def str: Type = TClass(ClassVar("Str"))

  test("provenance erases from concrete equality but never identifies fresh instances"):
    val original = binder("A", 1)
    val first = classes.declFreshVar(TypeVarKind.Flex, original)
    val second = classes.declFreshVar(TypeVarKind.Flex, original)
    assert(original == TypeVar("A"))
    assert(original.hashCode == TypeVar("A").hashCode)
    assert(first.var_ != second.var_)
    assert(first.var_.provenance == second.var_.provenance)
    assert(Set(first.var_, second.var_).size == 2)

  test("capture avoidance retains binder origin and preserves the free captured variable"):
    val original = binder("A", 1)
    val free = TypeVar("B")
    val scheme = TUniv(original, TLam(TVar(original), TVar(free)))
    scheme.substitute(free, TVar(original)) match
      case TUniv(renamed, TLam(TVar(parameter), TVar(result))) =>
        assert(renamed != original)
        assert(renamed == parameter)
        assert(result == original)
        assert(renamed.provenance == original.provenance)
      case other => fail(s"Unexpected capture-avoiding substitution: $other")

  test("opening a generalized instance retains its stable origin without an ancestry chain"):
    val original = binder("A", 1)
    val first = classes.declFreshVar(TypeVarKind.Flex, original)
    val again = classes.declFreshVar(TypeVarKind.Flex, first.var_)
    assert(first.var_ != again.var_)
    assert(first.var_.provenance == again.var_.provenance)

  test("abstract flow contexts accumulate marks idempotently"):
    val original = binder("A", 1)
    val mark = VarOrigin.Source(2)
    val ctx = classes.mapTrail(_.copy(flow = Set(mark)))
    val first = ctx.declFreshVar(TypeVarKind.Flex, original)
    val again = ctx.declFreshVar(TypeVarKind.Flex, first.var_)
    assert(again.var_.provenance.context == Set(original.provenance.root, mark))

  test("extrusion retains the root and distinguishes its directions"):
    val original = binder("A", 1)
    val first = classes.declFreshVar(TypeVarKind.Flex, original)
    val positive = classes.declExtrudeVar(first.var_, 0, Polarity.Positive)
    val negative = classes.declExtrudeVar(first.var_, 0, Polarity.Negative)
    assert(positive.var_.provenance.root == original.provenance.root)
    assert(positive.var_.provenance.context == first.var_.provenance.context)
    assert(positive.var_.provenance != negative.var_.provenance)

  test("a key recognizes fresh instances but retains their aliasing pattern"):
    val original = binder("A", 1)
    val a = classes.declFreshVar(TypeVarKind.Flex, original)
    val b = classes.declFreshVar(TypeVarKind.Flex, original)
    given SubContext = classes.extend(List(a, b))
    val aa = ProvenanceKey(TVar(a.var_), TLam(TVar(a.var_), int), Set.empty)
    val bb = ProvenanceKey(TVar(b.var_), TLam(TVar(b.var_), int), Set.empty)
    val ab = ProvenanceKey(TVar(a.var_), TLam(TVar(b.var_), int), Set.empty)
    assert(aa == bb)
    assert(aa != ab)

  test("equally named source binders have distinct provenance"):
    val a = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val b = classes.declFreshVar(TypeVarKind.Flex, binder("A", 2))
    given SubContext = classes.extend(List(a, b))
    assert(ProvenanceKey(TVar(a.var_), int, Set.empty) !=
      ProvenanceKey(TVar(b.var_), int, Set.empty))

  test("input variables and rigid variables retain concrete identities in keys"):
    val original = binder("A", 1)
    for kind <- List(TypeVarKind.Flex, TypeVarKind.Rigid) do
      val a = classes.declFreshVar(kind, original)
      val b = classes.declFreshVar(kind, original)
      given SubContext = classes.extend(List(a, b))
      val roots = if kind == TypeVarKind.Flex then Set(a.var_, b.var_) else Set.empty[TypeVar]
      assert(ProvenanceKey(TVar(a.var_), int, roots) != ProvenanceKey(TVar(b.var_), int, roots))

  test("changed side bounds are visible without recursively expanding their graph"):
    val a = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val ctx = classes.extend(a)
    val changed = ctx.extend(Bound(a.var_, Direction.Super, int, BoundKind.Asserted))
    val cyclic = ctx.extend(Bound(a.var_, Direction.Super, TVar(a.var_), BoundKind.Asserted))
    val key = ProvenanceKey(TVar(a.var_), str, Set.empty)(using ctx)
    assert(key != ProvenanceKey(TVar(a.var_), str, Set.empty)(using changed))
    assert(key != ProvenanceKey(TVar(a.var_), str, Set.empty)(using cyclic))

  test("abstract collisions never return a proof or a flexible bound"):
    config.provenancePolicy = ProvenancePolicy.Cut
    val a = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val b = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val ctx = classes.extend(List(a, b))
    val key = ProvenanceKey(TVar(a.var_), int, Set.empty)(using ctx)
    val active = ctx.mapTrail(_.copy(search = Some(SearchSession()), roots = Some(Set.empty),
      provenanceKeys = Set(key)))
    intercept[SearchInconclusive](subtype(TVar(b.var_), int)(using active))
    assert(!active.trail.contains(TVar(b.var_), int))

  test("an optional exhausted query is neither a proof nor a completed failure"):
    config.provenancePolicy = ProvenancePolicy.Cut
    config.checkSteps = Some(1)
    given SubContext = classes
    assert(checkSubtypeResult(TLam(int, int), TLam(int, str)) == SubtypeCheck.Inconclusive)
    assert(!checkSubtype(TLam(int, int), TLam(int, str)))

  test("an exhausted decided-type query cannot justify ex falso"):
    config.provenancePolicy = ProvenancePolicy.Cut
    config.exFalso = true
    config.checkSteps = Some(1)
    val a = TypeVar("A")
    val ctx = classes.declTypeVar(a, TypeVarKind.Rigid)
      .extend(Bound(a, Direction.Super, int, BoundKind.Asserted))
    val upper = TJointType(JointMode.Union, str, TClass(ClassVar("Bool")))
    assert(checkSubtypeResult(int, upper)(using ctx) == SubtypeCheck.Inconclusive)
    assert(ctx.assume(TVar(a), upper).isDefined)

  test("another successful alternative survives a local cut"):
    val result = classes.any(throw SearchInconclusive("test collision"), SubClauses.empty)
    assert(result == SubClauses.empty)

  test("an obligation-free proof makes further divergent alternatives unnecessary"):
    val result = classes.any(SubClauses.empty, throw SearchInconclusive("unneeded search", true))
    assert(result == SubClauses.empty)

  test("an obligation-free alternative removes earlier conditional solutions"):
    val a = TypeVar("A")
    val ctx = classes.declTypeVar(a, TypeVarKind.Flex)
    val result = ctx.any(subtype(TVar(a), int), SubClauses.empty)
    assert(result == SubClauses.empty)

  test("conditional alternatives retain both sets of witnesses"):
    val a = TypeVar("A")
    val ctx = classes.declTypeVar(a, TypeVarKind.Flex)
    val result = ctx.any(subtype(TVar(a), int), subtype(TVar(a), str))
    val upper = a.upperBound(using ctx.extend(result))
    assert(checkSubtype(int, upper)(using classes))
    assert(checkSubtype(str, upper)(using classes))
    assert(!checkSubtype(TClass(ClassVar("Bool")), upper)(using classes))

  test("unsuccessful alternatives preserve an inconclusive outcome"):
    intercept[SearchInconclusive]:
      classes.any(throw SearchInconclusive("test collision"), throw TypeError())

  test("global resource exhaustion is not swallowed by alternatives or optional queries"):
    intercept[SearchInconclusive]:
      classes.any(throw SearchInconclusive("test budget", true), SubClauses.empty)
    config.provenancePolicy = ProvenancePolicy.Cut
    config.searchSteps = Some(1)
    intercept[SearchInconclusive]:
      checkSubtype(TLam(int, int), TLam(int, str))(using classes)

  test("operation sessions do not leak into another operation"):
    config.provenancePolicy = ProvenancePolicy.Cut
    given SubContext = classes
    val first = withSearchSession(_.trail.search.get)
    val second = withSearchSession(_.trail.search.get)
    assert(first ne second)
    assert(summon[SubContext].trail.search.isEmpty)

  test("subtyping anchors instances created after the enclosing operation started"):
    config.provenancePolicy = ProvenancePolicy.Cut
    withSearchSession(active =>
      val a = active.declFreshVar(TypeVarKind.Flex, binder("A", 1))
      val b = active.declFreshVar(TypeVarKind.Flex, binder("A", 1))
      val inputs = active.extend(List(a, b))
      // An unanchored key of another argument instance must not cut this new solver entry.
      val key = ProvenanceKey(TVar(a.var_), int, Set.empty)(using inputs)
      val prepared = inputs.mapTrail(_.copy(provenanceKeys = Set(key)))
      val output = subtype(TVar(b.var_), int)(using prepared)
      assert(output.elems.exists:
        case Bound(variable, Direction.Sub, target, BoundKind.Asserted) =>
          variable == b.var_ && target == int
        case _ => false)
    )(using classes)

  test("observation records a provenance revisit without cutting or changing the result"):
    config.provenancePolicy = ProvenancePolicy.Observe
    val a = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val b = classes.declFreshVar(TypeVarKind.Flex, binder("A", 1))
    val ctx = classes.extend(List(a, b))
    val key = ProvenanceKey(TVar(a.var_), int, Set.empty)(using ctx)
    val session = SearchSession()
    val active = ctx.mapTrail(_.copy(search = Some(session), roots = Some(Set.empty),
      provenanceKeys = Set(key)))
    assert(subtype(TVar(b.var_), int)(using active).elems.nonEmpty)
    assert(session.abstractRepeats == 1)
    assert(session.cuts == 0)

  test("a fresh-instantiation loop is cut independently of resource budgets"):
    config.provenancePolicy = ProvenancePolicy.Cut
    val f = TypeVar("F")
    val x = binder("X", 1)
    val arrow = TLam(TVar(x), TVar(x))
    val scheme = TUniv(x, TConstrained(arrow, Constraint(TVar(f), Direction.Sub, arrow)))
    val session = SearchSession()
    val ctx = classes.declTypeVar(f, TypeVarKind.Rigid)
      .extend(Bound(f, Direction.Sub, scheme, BoundKind.Asserted))
      .mapTrail(_.copy(search = Some(session)))
    val error = intercept[SearchInconclusive](subtype(TVar(f), int)(using ctx))
    assert(error.reason.contains("provenance"))
    assert(session.freshCount == 2)
    assert(session.abstractRepeats == 1)

  test("bounded syntax recognizes growing target witnesses"):
    config.provenancePolicy = ProvenancePolicy.Cut
    val f = TypeVar("F")
    val x = binder("X", 1)
    val arrow = TLam(TVar(x), TVar(x))
    val scheme = TUniv(x, TConstrained(TVar(x), Constraint(TVar(f), Direction.Sub, arrow)))
    val session = SearchSession()
    val ctx = classes.declTypeVar(f, TypeVarKind.Rigid)
      .extend(Bound(f, Direction.Sub, scheme, BoundKind.Asserted))
      .mapTrail(_.copy(search = Some(session)))
    intercept[SearchInconclusive](subtype(TVar(f), int)(using ctx))
    assert(session.abstractRepeats > 0)
    assert(session.steps < 1000)

  test("an inconclusive target witness leaves ordinary quantified instantiation available"):
    config.provenancePolicy = ProvenancePolicy.Cut
    val a = TypeVar("A")
    val x = binder("X", 1)
    val ctx = classes.declTypeVar(a, TypeVarKind.Rigid)
      .extend(Bound(a, Direction.Sub, TLam(int, int), BoundKind.Asserted))
    val key = ProvenanceKey(TVar(a), TLam(int, int), Set(a))(using ctx)
    val session = SearchSession()
    val active = ctx.mapTrail(_.copy(search = Some(session), roots = Some(Set(a)),
      provenanceKeys = Set(key)))
    val scheme = TUniv(x, TConstrained(TVar(x),
      Constraint(TVar(a), Direction.Sub, TLam(TVar(x), TVar(x)))))
    subtype(scheme, int)(using active)
    assert(session.cuts == 1)
    assert(session.freshCount == 1)

  test("resource limits are independently usable with provenance detection disabled"):
    config.provenancePolicy = ProvenancePolicy.Off
    config.searchSteps = Some(1)
    intercept[SearchInconclusive](subtype(TLam(int, int), TLam(int, str))(using classes))

  test("an inconclusive query does not request more class case splits"):
    config.checkSteps = Some(1)
    given SubContext = classes
    val both = TJointType(JointMode.Union, int, str)
    assert(checkSubtypeResult(both, TClass(ClassVar("Bool"))) == SubtypeCheck.Inconclusive)
    assert(!both.isSplitBy(TClass(ClassVar("Bool"))))
