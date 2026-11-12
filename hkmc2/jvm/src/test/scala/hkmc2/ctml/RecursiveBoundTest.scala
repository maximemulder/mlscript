package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Semantic and solver checks for Boolean self-bound elimination. */
class RecursiveBoundTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Isolate solver policies and generated names. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()

  /** Restore the default policy for subsequent suites. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** The variable whose self occurrences are eliminated. */
  private val x = TypeVar("X")

  /** An independent Boolean atom, also used underneath structural constructors. */
  private val a = TypeVar("A")

  /** Interpret a Boolean formula at one value; opaque types have independent memberships. */
  private def member(type_ : Type, valuation: Map[Type, Boolean]): Boolean = type_ match
    case TTop => true
    case TBot => false
    case TNeg(body) => !member(body, valuation)
    case TJointType(JointMode.Union, left, right) =>
      member(left, valuation) || member(right, valuation)
    case TJointType(JointMode.Inter, left, right) =>
      member(left, valuation) && member(right, valuation)
    case _ => valuation(type_)

  test("normalization preserves both bound directions for all Boolean valuations"):
    val atoms = List(TVar(x), TVar(a), TField("tail", TVar(x)))
    val leaves = atoms ++ List(TTop, TBot)
    val small = leaves ++ leaves.map(TNeg(_)) ++ (for
      mode <- JointMode.values.toList
      left <- leaves
      right <- leaves
    yield TJointType(mode, left, right))
    val formulas = small ++ small.map(TNeg(_)) ++ (for
      mode <- JointMode.values.toList
      left <- small
      right <- small
    yield TJointType(mode, left, right))
    for
      dir <- Direction.values
      formula <- formulas
      bits <- 0 until (1 << atoms.size)
    do
      val valuation = atoms.zipWithIndex.map((atom, i) => atom -> ((bits & (1 << i)) != 0)).toMap
      val original = member(formula, valuation)
      val normalized = member(normalizeRecursiveBound(x, dir, formula), valuation)
      val xv = valuation(TVar(x))
      val holds = if dir == Direction.Sub then !xv || original else !original || xv
      val simplified = if dir == Direction.Sub then !xv || normalized else !normalized || xv
      assert(holds == simplified, s"$dir: $formula at $valuation")

  test("structural payloads and binder scopes remain opaque"):
    val types = List(
      TField("tail", TVar(x)), TTuple(TVar(x), TVar(a)), TLam(TVar(x), TVar(x)),
      TApp(TVar(a), TVar(x)), TUniv(x, TVar(x)),
      TConstrained(TVar(x), Constraint(TVar(x), Direction.Sub, TVar(a))))
    for dir <- Direction.values; type_ <- types do
      assert(normalizeRecursiveBound(x, dir, type_) == type_)

  test("a negative lower self bound forces Top and conflicts with an upper Bottom bound"):
    for policy <- ProvenancePolicy.values do
      config.provenancePolicy = policy
      val ctx = SubContext.empty.declTypeVar(x, TypeVarKind.Flex)
      val lower = subtype(TNeg(TVar(x)), TVar(x))(using ctx)
      val bounded = ctx.extend(lower)
      assert(x.lowerBound(using bounded) == TTop)
      intercept[TypeError](subtype(TVar(x), TBot)(using bounded))

  test("a negative upper self bound forces Bottom and conflicts with a lower Top bound"):
    val ctx = SubContext.empty.declTypeVar(x, TypeVarKind.Flex)
    val upper = subtype(TVar(x), TNeg(TVar(x)))(using ctx)
    val bounded = ctx.extend(upper)
    assert(x.upperBound(using bounded) == TBot)
    intercept[TypeError](subtype(TTop, TVar(x))(using bounded))

  test("normalization respects concrete identity when instances share provenance"):
    val ctx = SubContext.empty
    val first = ctx.declFreshVar(TypeVarKind.Flex, x)
    val second = ctx.declFreshVar(TypeVarKind.Flex, x)
    assert(first.var_.provenance == second.var_.provenance)
    val bound = TNeg(TVar(second.var_))
    assert(normalizeRecursiveBound(first.var_, Direction.Super, bound) == bound)

  test("rigid assumptions expose their Boolean consequences without refuting a search failure"):
    val ctx = SubContext.empty.declTypeVar(x, TypeVarKind.Rigid)
    val lower = ctx.assume(TNeg(TVar(x)), TVar(x)).get
    val upper = ctx.assume(TVar(x), TNeg(TVar(x))).get
    assert(checkSubtype(TTop, TVar(x))(using lower))
    assert(checkSubtype(TVar(x), TBot)(using upper))
