package hkmc2.ctml.core.subtyping

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.var_.{declClass, declTypeVar}
import hkmc2.ctml.types.*

/** Check the obligations returned to the caller when comparing quantified functions and fields. */
class ForallDistributionTest extends AnyFunSuite:
  test("distributing an identity field retains the outer function's domain obligation"):
    val x = TypeVar("X")
    val y = TypeVar("Y")
    val p = TypeVar("P")
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    given ctx: SubContext = SubContext.empty.declClass("Int", None).declClass("Str", None)
      .declTypeVar(p, TypeVarKind.Flex)
    val source = TUniv(x, TLam(int, TField("id", TLam(TVar(x), TVar(x)))))
    val target = TLam(TVar(p), TField("id", TUniv(y, TLam(TVar(y), TVar(y)))))
    val clauses = subtype(source, target)
    assert(clauses.elems.collect { case bound: Bound => bound }.forall(_.var_ == p))
    val solved = ctx.extend(clauses)
    subtype(int, TVar(p))(using solved)
    intercept[TypeError](subtype(str, TVar(p))(using solved))

  test("ordinary instantiation retains the caller's domain and guard obligations"):
    val p = TypeVar("P")
    val q = TypeVar("Q")
    val x = TypeVar("X")
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    given ctx: SubContext = SubContext.empty.declClass("Int", None).declClass("Str", None)
      .declTypeVar(p, TypeVarKind.Flex).declTypeVar(q, TypeVarKind.Flex)
    val body = TConstrained(TVar(x), Constraint(TVar(q), Direction.Sub, TVar(x)))
    val source = TUniv(x, TLam(TVar(x), body))
    val solved = ctx.extend(subtype(source, TLam(TVar(p), int)))
    for caller <- List(p, q) do
      subtype(int, TVar(caller))(using solved)
      intercept[TypeError](subtype(str, TVar(caller))(using solved))

  test("a monomorphic result keeps ordinary instantiation available for a flexible caller"):
    val x = TypeVar("X")
    val p = TypeVar("P")
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    given ctx: SubContext = SubContext.empty.declClass("Int", None).declClass("Str", None)
      .declTypeVar(p, TypeVarKind.Flex)
    val source = TUniv(x, TLam(int, TJointType(JointMode.Union, TVar(p), TVar(x))))
    val target = TLam(TVar(p), TJointType(JointMode.Union, TLam(int, int), TVar(p)))
    val solved = ctx.extend(subtype(source, target))
    subtype(int, TVar(p))(using solved)
    intercept[TypeError](subtype(str, TVar(p))(using solved))
