package hkmc2.ctml

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.{makeConstrainedType, makeJointType}
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.{Hypotheses, SubtypingTrail}
import hkmc2.ctml.types.*

/** Equality of type syntax preserves lexical scope and agrees with collection lookup. */
class TypeSyntaxTest extends AnyFunSuite:
  /** Check direct equality and hashed lookup in both directions. */
  private def compare(left: Type, right: Type, equal: Boolean): Unit =
    val l = TypeSyntaxKey(left)
    val r = TypeSyntaxKey(right)
    assert(sameTypeSyntax(left, right) == equal)
    assert(sameTypeSyntax(right, left) == equal)
    assert((l == r) == equal)
    assert(Set(l, r).size == (if equal then 1 else 2))
    if equal then assert(l.hashCode == r.hashCode)

  test("binding position distinguishes free, captured and shadowed uses even in a shared node"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val shared = TVar(a)
    compare(TUniv(a, shared), TUniv(b, shared), false)
    compare(TUniv(a, TUniv(a, shared)), TUniv(a, TUniv(b, TVar(b))), true)
    compare(TUniv(a, TUniv(a, shared)), TUniv(a, TUniv(b, shared)), false)
    compare(TTuple(TUniv(a, shared), shared), TTuple(TUniv(b, TVar(b)), shared), true)
    val boundAndFree = TTuple(TUniv(a, shared), TUniv(b, shared))
    val bothBound = TTuple(TUniv(a, shared), TUniv(b, TVar(b)))
    compare(boundAndFree, bothBound, false)
    compare(TVar(a), TVar(b), false)

  test("constructor labels, guard direction and operand order are part of syntax"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    /** A field and both sides of its guard sit under the same binder. */
    def scheme(v: TypeVar, direction: Direction, label: String): Type =
      val varType = TVar(v)
      TUniv(v, TConstrained(TField(label, varType), Constraint(TTop, direction, varType)))
    compare(scheme(a, Direction.Sub, "f"), scheme(b, Direction.Sub, "f"), true)
    compare(scheme(a, Direction.Sub, "f"), scheme(b, Direction.Super, "f"), false)
    compare(scheme(a, Direction.Sub, "f"), scheme(b, Direction.Sub, "g"), false)
    compare(TJointType(JointMode.Union, TTop, TBot), TJointType(JointMode.Union, TBot, TTop), false)
    compare(TApp(TBot, TTop), TLam(TBot, TTop), false)

  test("hashed lookup and simplification recognize a renamed binder and preserve source syntax"):
    val a = TypeVar("A")
    val b = TypeVar("B")
    val first = TUniv(a, TVar(a))
    val second = TUniv(b, TVar(b))
    val c = TypeVar("C")
    compare(TTuple(first, first), TTuple(second, TUniv(c, TVar(c))), true)
    val fillers = (1 to 8).map(i => TypeSyntaxKey(TVar(TypeVar(s"Free$i"))))
    val table = (fillers :+ TypeSyntaxKey(first)).toSet
    assert(table.contains(TypeSyntaxKey(second)))
    val assumptions = Hypotheses().add(TTop, first)
    assert(assumptions.contains(TTop, second))
    assert(assumptions.add(TTop, second).judgments.size == 1)
    assert(makeJointType(JointMode.Union, first, second) == second)
    val guard = Constraint(first, Direction.Sub, TBot)
    assert(makeConstrainedType(TBot, List(guard, guard.copy(left = second))) ==
      TConstrained(TBot, guard))
    val owner = TVar(TypeVar("Owner"))
    val trail = SubtypingTrail().add(owner, first)
    assert(trail.contains(owner, first))
    assert(!trail.contains(owner, second))

  test("deep shared syntax stays compact during comparison and hashing"):
    /** The unfolded tree contains two copies of the previous level at every step. */
    def doubled(name: String): Type =
      val variable = TypeVar(name)
      TUniv(variable, (0 until 2048).foldLeft[Type](TVar(variable))((t, _) => TLam(t, t)))
    compare(doubled("A"), doubled("B"), true)
