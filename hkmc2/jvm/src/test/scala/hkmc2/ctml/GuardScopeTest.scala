package hkmc2.ctml

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.*
import hkmc2.ctml.core.simplification.simplify
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.var_.declTypeVar
import hkmc2.ctml.types.*

/** Check binder scope during guard construction and arrow simplification. */
class GuardScopeTest extends AnyFunSuite:
  /** The nonempty finite universe used only for Boolean and universal type interpretation. */
  private val universe = Set(0, 1)

  /** Every possible interpretation of one variable in this finite model. */
  private val subsets = List(Set.empty[Int], Set(0), Set(1), universe)

  /** Interpret universal types by intersecting all local assignments, preserving lexical scope. */
  private def denote(t: Type, env: Map[TypeVar, Set[Int]]): Set[Int] = t match
    case TTop => universe
    case TBot => Set.empty
    case TVar(v) => env(v)
    case TClass(ClassVar("Int")) => Set(0)
    case TClass(ClassVar("Str")) => Set(1)
    case TNeg(body) => universe -- denote(body, env)
    case TJointType(JointMode.Inter, left, right) => denote(left, env) intersect denote(right, env)
    case TJointType(JointMode.Union, left, right) => denote(left, env) union denote(right, env)
    case TUniv(v, body) => subsets.map(value => denote(body, env.updated(v, value)))
      .reduce(_ intersect _)
    case TConstrained(body, guard) =>
      val left = denote(guard.left, env)
      val right = denote(guard.right, env)
      val holds = if guard.dir == Direction.Sub then left.subsetOf(right) else right.subsetOf(left)
      if holds then denote(body, env) else universe
    case other => fail(s"Outside the finite Boolean/universal model: $other")

  test("guard movement preserves all finite valuations across shadowing binders"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    val guards = List(
      Constraint(TVar(a), Direction.Sub, int),
      Constraint(TVar(b), Direction.Sub, str),
      Constraint(TVar(a), Direction.Super, TVar(b)),
      Constraint(TUniv(b, TVar(b)), Direction.Sub, TVar(a)))
    val bodies = List(
      TUniv(a, int), TUniv(b, str), TUniv(a, TVar(a)),
      TUniv(a, TUniv(b, TJointType(JointMode.Union, TVar(a), TVar(b)))),
      TUniv(b, TConstrained(int, Constraint(TVar(a), Direction.Sub, TVar(b)))))
    for
      body <- bodies
      first <- guards
      second <- guards
      constraining <- List(false, true)
    do
      val constraints = List(first, second)
      val rawBody = if constraining then TNeg(body) else body
      val rawGuarded = constraints.foldRight(rawBody: Type)((guard, t) => TConstrained(t, guard))
      val expected = if constraining then TNeg(rawGuarded) else rawGuarded
      val actual = if constraining then makeConstrainingType(body, constraints)
        else makeConstrainedType(body, constraints)
      for av <- subsets; bv <- subsets do
        val env = Map(a -> av, b -> bv)
        assert(denote(actual, env) == denote(expected, env),
          s"body=$body guards=$constraints constraining=$constraining env=$env actual=$actual")

  test("simplification preserves a polymorphic result under its original arrow"):
    val a = TypeVar("A")
    given SubContext = SubContext.empty.declTypeVar(a, TypeVarKind.Rigid)
    val result = TUniv(a, TLam(TVar(a), TVar(a)))
    for param <- List(TTop, TVar(a)) do
      val arrow = TLam(param, result)
      assert(arrow.simplify() == arrow)

  test("independent guards permit binder movement"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val body = TUniv(a, TVar(a))
    val guard = Constraint(TVar(b), Direction.Sub, TClass(ClassVar("Int")))
    assert(makeConstrainedType(body, List(guard)).getVars == Set(b))
    assert(makeConstrainingType(body, List(guard)).getVars == Set(b))
