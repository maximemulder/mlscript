package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Check solution preservation separately from the ordinary solver's ability to find a proof. */
class RecursiveClosureTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Reset the solver and bound every required query in this experiment. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()
    config.searchSteps = Some(10000)

  /** Restore defaults for other suites. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** The sole variable owned by a local constraint problem. */
  private val a = TypeVar("A")

  /** Its declaration is outside the independent ambient context. */
  private val local = TypeVarDecl(a, TypeVarKind.Flex, None, 0)

  /** A positive structural operator, independent of aliases and type application variance. */
  private def layer(t: Type): Type = TField("tail", t)

  /** A lower bound in the surface guard orientation. */
  private def lower(t: Type): Constraint = Constraint(TVar(a), Direction.Super, t)

  /** Construct the existing encoding, without any dedicated recursive type constructor. */
  private def mu(f: Type => Type): Type =
    val x = TypeVar("X")
    TUniv(x, TConstrained(TVar(x), Constraint(f(TVar(x)), Direction.Sub, TVar(x))))

  test("substitution covers results, both constraint operands, and negative positions"):
    val result = TLam(TVar(a), TTuple(TVar(a), TNeg(TVar(a))))
    val remaining = List(Constraint(TVar(a), Direction.Sub, TTop),
      Constraint(TField("tail", TVar(a)), Direction.Sub, TNeg(TVar(a))))
    val closed = closeRecursiveVariable(local, result, lower(layer(TVar(a))) :: remaining)
      (using SubContext.empty).get
    assert(closed.result == result.substitute(a, closed.witness))
    assert(closed.constraints == remaining.map(_.substitute(a, closed.witness)))
    assert(!closed.result.hasVar(a))
    assert(!closed.constraints.exists(_.hasVar(a)))
    assert(closed.witness.hasVar(closed.parameter.var_))
    assert(closed.parameter.level == local.level)

  test("all direct monotone lower bounds are included regardless of order or orientation"):
    val guards = List(lower(layer(TVar(a))),
      Constraint(TField("other", TVar(a)), Direction.Sub, TVar(a)), lower(TTop))
    for ordered <- guards.permutations do
      val closed = closeRecursiveVariable(local, TVar(a), ordered)(using SubContext.empty).get
      assert(closed.constraints.isEmpty)
      assert(checkSubtype(TTop, closed.witness.substitute(closed.parameter.var_, TBot))
        (using SubContext.empty))

  test("unsupported recursion stays an obligation rather than supplying a proof"):
    val negative = lower(TField("tail", TNeg(TVar(a))))
    val guarded = lower(TConstrained(TVar(a), Constraint(TTop, Direction.Sub, TVar(a))))
    val quantified = lower(TUniv(TypeVar("Z"), layer(TVar(a))))
    for guard <- List(negative, guarded, quantified) do
      assert(closeRecursiveVariable(local, TVar(a), List(guard))(using SubContext.empty).isEmpty)
      val closed = closeRecursiveVariable(local, TVar(a), List(lower(layer(TVar(a))), guard))
        (using SubContext.empty).get
      assert(closed.constraints == List(guard.substitute(a, closed.witness)))

  test("rigid variables, ambient dependencies, and later parameters cannot be closed"):
    val guards = List(lower(layer(TVar(a))))
    assert(closeRecursiveVariable(local.copy(kind = TypeVarKind.Rigid), TVar(a), guards)
      (using SubContext.empty).isEmpty)
    intercept[AssertionError]:
      closeRecursiveVariable(local, TVar(a), guards)
        (using SubContext.empty.declTypeVar(a, TypeVarKind.Flex))
    val later = TypeVar("Later")
    val ctx = SubContext.empty.mapLevel(_ => 1).declTypeVar(later, TypeVarKind.Rigid)
    assert(closeRecursiveVariable(local, TVar(a), guards :+ lower(TVar(later)))(using ctx).isEmpty)

  test("the seed retains larger solutions even when the result uses the carrier negatively"):
    val closed = closeRecursiveVariable(local, TLam(TVar(a), TVar(a)),
      List(lower(layer(TVar(a)))))(using SubContext.empty).get
    val largest = closed.witness.substitute(closed.parameter.var_, TTop)
    assert(checkSubtype(TTop, largest)(using SubContext.empty))
    val smallest = closed.witness.substitute(closed.parameter.var_, TBot)
    assert(!checkSubtype(TTop, smallest)(using SubContext.empty))

  test("the solver uses the closure in arrows, fields, tuples, and negative occurrences"):
    config.recursiveClosure = true
    val rec = mu(layer)
    val shapes: List[Type => Type] = List(
      t => TLam(TTop, t), t => TField("value", t), t => TTuple(t, t),
      t => TLam(t, TTop))
    for shape <- shapes do
      val scheme = TUniv(a, TConstrained(shape(layer(TVar(a))), lower(layer(TVar(a)))))
      val target = shape(rec)
      val clauses = subtype(scheme, target)(using SubContext.empty)
      assert(!clauses.elems.exists {
        case Bound(_, _, bound, _) => bound.hasVar(a)
        case _ => false
      })

  test("an already opened local problem has the same closure solution as its quantified form"):
    val rec = mu(layer)
    val result = TTuple(layer(TVar(a)), TLam(TVar(a), TTop))
    val guards = List(lower(layer(TVar(a))))
    val closed = closeRecursiveVariable(local, result, guards)(using SubContext.empty).get
    val target = TTuple(rec, TLam(rec, TTop))
    // No universal binder or producer shape is passed to the transformation.
    subtype(closed.guardedResult, target)
      (using SubContext.empty.extend(closed.parameter))
    config.recursiveClosure = true
    subtype(TUniv(a, TConstrained(result, guards.head)), target)(using SubContext.empty)

  test("target intersections can use different instances of a polymorphic carrier"):
    val scheme = TUniv(a, TConstrained(TLam(TVar(a), TVar(a)), lower(layer(TVar(a)))))
    val target = TJointType(JointMode.Inter,
      TLam(TTop, TTop), TLam(layer(TTop), layer(TTop)))
    for enabled <- List(false, true) do
      config.recursiveClosure = enabled
      subtype(scheme, target)(using SubContext.empty)

  test("contradictory extra upper bounds are retained in either guard order"):
    config.recursiveClosure = true
    val ctx = SubContext.empty.declClass("Seed", None).declClass("Other", None)
    val seed = TClass(ClassVar("Seed"))
    val other = TClass(ClassVar("Other"))
    val recursive = lower(TJointType(JointMode.Union, seed, layer(TVar(a))))
    val upper = Constraint(TVar(a), Direction.Sub, other)
    for guards <- List(List(recursive, upper), List(upper, recursive)) do
      val instance = guards.foldRight[Type](TLam(TTop, TTop))((c, t) => TConstrained(t, c))
      intercept[TypeError](subtype(TUniv(a, instance), TLam(TTop, TTop))(using ctx))

  test("a checked concrete seed satisfies every residual bound of a positive producer"):
    config.recursiveClosure = true
    val scheme = TUniv(a, TConstrained(TLam(TTop, layer(TVar(a))), lower(layer(TVar(a)))))
    val clauses = subtype(scheme, TLam(TTop, mu(layer)))(using SubContext.empty)
    val declarations = clauses.elems.collect { case d: TypeVarDecl => d }
    val checking = SubContext.empty.extend(declarations)
    val flexible = declarations.filter(_.kind == TypeVarKind.Flex).map(_.var_)
    for case Bound(variable, direction, bound, BoundKind.Asserted) <- clauses.elems do
      val constraint = flexible.foldLeft(Constraint(TVar(variable), direction, bound))((c, v) =>
        c.substitute(v, TBot))
      assert(checkSubtypeDir(constraint.left, constraint.right, constraint.dir)(using checking),
        s"unverified residual obligation: $constraint")

  test("all 8000 monotone maps on three atoms preserve the entire set of solutions"):
    val values = 0 until 8
    /** Inclusion in a three-atom powerset, represented by bits. */
    def subset(x: Int, y: Int): Boolean = (x & ~y) == 0
    val booleanMaps = (0 until 256).filter(mask => values.forall(x => values.forall(y =>
      !subset(x, y) || ((mask >> x) & 1) <= ((mask >> y) & 1))))
    assert(booleanMaps.size == 20)
    for m0 <- booleanMaps; m1 <- booleanMaps; m2 <- booleanMaps do
      /** Interpret the three coordinate-wise monotone membership functions. */
      def f(x: Int): Int = ((m0 >> x) & 1) | (((m1 >> x) & 1) << 1) |
        (((m2 >> x) & 1) << 2)
      /** Compute the least fixed point of `f(x) | seed` in this finite lattice. */
      def close(seed: Int): Int =
        var x = 0
        var next = f(x) | seed
        while next != x do
          x = next
          next = f(x) | seed
        x
      val solutions = values.filter(x => subset(f(x), x)).toSet
      val represented = values.map(close).toSet
      assert(represented == solutions)
      // Every possible residual predicate, including nonmonotone ones, sees the same solutions.
      for predicate <- 0 until 256 do
        assert(solutions.exists(x => ((predicate >> x) & 1) == 1) ==
          represented.exists(x => ((predicate >> x) & 1) == 1))
