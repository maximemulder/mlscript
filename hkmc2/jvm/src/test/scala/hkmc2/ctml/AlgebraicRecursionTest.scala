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

/** Check the algebraic transformation separately from the incomplete downstream proof search. */
class AlgebraicRecursionTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Keep every test independent and bound solver work. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    config.algebraicRecursion = true
    config.searchSteps = Some(10000)

  /** Restore the configuration for other suites. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** A local unknown with no dependency on an enclosing inference scope. */
  private val a = TypeVar("A")

  /** The declaration owned by the problem rather than its ambient context. */
  private val local = TypeVarDecl(a, TypeVarKind.Flex, None, 0)

  /** A structural monotone recurrence. */
  private def layer(t: Type): Type = TField("step", t)

  test("upper closure substitutes the complete problem and retains opposite bounds"):
    val recurrence = Constraint(TVar(a), Direction.Sub, layer(TVar(a)))
    val other = Constraint(TBot, Direction.Sub, TVar(a))
    val result = TLam(TVar(a), TNeg(TVar(a)))
    for guard <- List(recurrence, Constraint(layer(TVar(a)), Direction.Super, TVar(a))) do
      val closed = closeRecursiveVariable(local, result, List(guard, other), Direction.Sub)
        (using SubContext.empty).get
      assert(closed.result == result.substitute(a, closed.witness))
      assert(closed.constraints == List(other.substitute(a, closed.witness)))
      assert(!closed.guardedResult.hasVar(a))
      assert(closed.parameter.level == local.level)

  test("the actual encoded witnesses represent every solution for all 8000 monotone maps"):
    val values = 0 until 8
    /** Set inclusion in a three-atom Boolean algebra. */
    def subset(x: Int, y: Int): Boolean = (x & ~y) == 0
    val maps = (0 until 256).filter(mask => values.forall(x => values.forall(y =>
      !subset(x, y) || ((mask >> x) & 1) <= ((mask >> y) & 1))))
    assert(maps.size == 20)
    val closures = Direction.both.map: direction =>
      val guard = Constraint(TVar(a), direction, layer(TVar(a)))
      (direction, closeRecursiveVariable(local, TVar(a), List(guard), direction)
        (using SubContext.empty).get)
    for m0 <- maps; m1 <- maps; m2 <- maps do
      /** Interpret a field as an arbitrary monotone operation, not a particular list encoding. */
      def f(x: Int): Int = ((m0 >> x) & 1) | (((m1 >> x) & 1) << 1) |
        (((m2 >> x) & 1) << 2)
      /** Interpret the generated universal-and-guard syntax directly. */
      def eval(t: Type, env: Map[TypeVar, Int]): Int = t match
        case TBot => 0
        case TTop => 7
        case TVar(v) => env(v)
        case TNeg(t) => 7 ^ eval(t, env)
        case TField("step", t) => f(eval(t, env))
        case TJointType(JointMode.Union, l, r) => eval(l, env) | eval(r, env)
        case TJointType(JointMode.Inter, l, r) => eval(l, env) & eval(r, env)
        case TUniv(v, body) => values.foldLeft(7)((acc, value) => acc & eval(body, env + (v -> value)))
        case TConstrained(body, Constraint(left, direction, right)) =>
          val (l, r) = (eval(left, env), eval(right, env))
          val holds = if direction == Direction.Sub then subset(l, r) else subset(r, l)
          if holds then eval(body, env) else 7
        case _ => fail(s"unexpected syntax in closure witness: $t")
      for (direction, closed) <- closures do
        val expected = values.filter(x =>
          if direction == Direction.Sub then subset(x, f(x)) else subset(f(x), x)).toSet
        val actual = values.map(seed => eval(closed.witness, Map(closed.parameter.var_ -> seed))).toSet
        assert(actual == expected)

  test("complete problems close independent producer and consumer carriers together"):
    val b = TypeVar("B")
    val declB = local.copy(var_ = b)
    val result = TLam(TVar(a), TVar(b))
    val guards = List(Constraint(TVar(a), Direction.Sub, layer(TVar(a))),
      Constraint(layer(TVar(b)), Direction.Sub, TVar(b)))
    val normalized = closeRecursiveProblem(RecursiveProblem(List(local, declB), result, guards))
      (using SubContext.empty).get
    assert(normalized.constraints.isEmpty)
    assert(!normalized.result.hasVar(a) && !normalized.result.hasVar(b))
    assert(normalized.declarations.size == 2)
    assert(normalized.result.getVars == normalized.declarations.map(_.var_).toSet)

  test("a free payload before a recursive carrier does not hide its closure opportunity"):
    val b = TypeVar("B")
    val body = TJointType(JointMode.Union, TVar(a), layer(TVar(b)))
    val recursive = Constraint(body, Direction.Sub, TVar(b))
    val normalized = closeRecursiveProblem(RecursiveProblem(List(local, local.copy(var_ = b)),
      TVar(b), List(recursive)))(using SubContext.empty).get
    assert(normalized.declarations.exists(_.var_ == a))
    assert(!normalized.result.hasVar(b))
    assert(normalized.result.hasVar(a))

  test("negative and guarded upper recursion remain unsupported obligations"):
    val guarded = TConstrained(TVar(a), Constraint(TVar(a), Direction.Sub, TBot))
    for body <- List(layer(TNeg(TVar(a))), guarded) do
      val constraint = Constraint(TVar(a), Direction.Sub, body)
      assert(closeRecursiveProblem(RecursiveProblem(List(local), TVar(a), List(constraint)))
        (using SubContext.empty).isEmpty)

  test("opposite inconsistent bounds cannot disappear when an upper carrier is closed"):
    val ctx = SubContext.empty.declClass("Atom", None)
    val atom = TClass(ClassVar("Atom"))
    val upper = Constraint(TVar(a), Direction.Sub,
      TJointType(JointMode.Inter, atom, layer(TVar(a))))
    val lower = Constraint(TTop, Direction.Sub, TVar(a))
    val scheme = TUniv(a, TConstrained(TConstrained(TVar(a), upper), lower))
    intercept[TypeError](subtype(scheme, atom)(using ctx))

  test("recursive propagation checks opposite bounds discovered while the new bound is pending"):
    config.algebraicRecursion = false
    val b = TypeVar("B")
    val classes = SubContext.empty.declClass("Stop", None).declClass("Next", None)
      .declClass("Bool", None)
    val ctx = classes.extend(List(local, local.copy(var_ = b)))
    val stop = TClass(ClassVar("Stop"))
    val bool = TClass(ClassVar("Bool"))
    /** A tagged constructor distinguishes malformed tails from empty fields. */
    def next(t: Type): Type = TJointType(JointMode.Inter, TClass(ClassVar("Next")), TField("tail", t))
    val upper = Constraint(TVar(a), Direction.Sub, TJointType(JointMode.Union, stop, next(TVar(a))))
    for leaf <- List(stop, bool) do
      val guards = List(Constraint(leaf, Direction.Sub, TVar(b)),
        Constraint(next(TVar(b)), Direction.Sub, TVar(a)), upper)
      for ordered <- guards.permutations do
        /** Solve the same finite problem in each constraint order. */
        def solve(): SubClauses = ordered.foldLeft(SubClauses.empty)((outs, c) =>
          subtypeConstraintSeq(c, outs)(using ctx))
        if leaf == bool then intercept[TypeError](solve())
        else solve()
