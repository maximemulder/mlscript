package hkmc2.ctml.core.subtyping

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.var_.{declClass, declTypeVar}
import hkmc2.ctml.types.*

/** Exact arrow splits and the caller bounds left by guarded callbacks. */
class ArrowSplitTest extends AnyFunSuite:
  private val int = TClass(ClassVar("Int"))
  private val str = TClass(ClassVar("Str"))
  private val bool = TClass(ClassVar("Bool"))
  private val d = TypeVar("D")
  private val p = TypeVar("P")
  private val a = TypeVar("A")
  private val base = SubContext.empty.declClass("Int", None).declClass("Str", None)
    .declClass("Bool", None).declTypeVar(d, TypeVarKind.Flex).declTypeVar(p, TypeVarKind.Flex)

  /** Lower-bound the caller before checking the callback. A later argument may grow it again. */
  private def caller =
    base.extend(subtype(TJointType(JointMode.Union, int, str), TVar(d))(using base))

  /** The result is available when the quantified parameter lies within the test type. */
  private def arm(test: Type, result: Type = int) =
    TConstrained(result, Constraint(TVar(a), Direction.Sub, test))

  /** Check the same source and target as a callback passed by the surface-language fixture. */
  private def solveResult(body: Type, result: Type = int)(using SubContext): SubClauses =
    subtype(TUniv(a, TLam(TVar(a), body)), TLam(TVar(d), result))

  test("complementary guards cover any later caller with no residual bounds"):
    for ctx <- List(base, caller); pivot <- List(bool, TField("x", int), TVar(p)) do
      for (l, r) <- List((pivot, TNeg(pivot)), (TNeg(pivot), pivot)) do
        val solved = solveResult(TJointType(JointMode.Inter, arm(l), arm(r)))(using ctx)
        assert(solved.elems.isEmpty)
        subtype(TTop, TVar(d))(using ctx.extend(solved))

  test("an extra promise of a different result can separate an exhaustive pair"):
    val q = TVar(p)
    val body = TJointType(JointMode.Inter, TJointType(JointMode.Inter, arm(q), arm(int, str)),
      arm(TNeg(q)))
    assert(solveResult(body)(using caller).elems.isEmpty)

  test("exact splitting exposes fields and arrows with opaque variables"):
    val field = TField("x", TJointType(JointMode.Union, TVar(p), str))
    val target = TLam(field, int)
    assert(target.splitExact(JointMode.Inter).isEmpty)
    assert(target.splitExact(JointMode.Inter, expandProducts = true).contains(
      (TLam(TField("x", TVar(p)), int), TLam(TField("x", str), int))))
    assert(TNeg(TNeg(TNeg(field))).splitExact(JointMode.Inter).contains(
      (TNeg(TField("x", TVar(p))), TNeg(TField("x", str)))))
    assert(TVar(d).splitExact(JointMode.Union, expandProducts = true).isEmpty)

  test("three partial promises retain exactly the covered classes for a later argument"):
    val ctx = caller
    for ordered <- List(int, str, bool).permutations do
      val arms: List[Type] = ordered.map(arm(_))
      for body <- List(
        arms.reduceLeft(TJointType(JointMode.Inter, _, _)),
        arms.reduceRight(TJointType(JointMode.Inter, _, _)),
      ) do
        val solved = ctx.extend(solveResult(body)(using ctx))
        for input <- ordered do subtype(input, TVar(d))(using solved)
        intercept[TypeError](subtype(TTop, TVar(d))(using solved))

  test("an uncovered domain cannot acquire the result promised by its neighbor"):
    val ctx = caller
    val source = TJointType(JointMode.Inter, arm(bool), arm(TNeg(bool), str))
    intercept[TypeError](solveResult(source)(using ctx))

  test("a guard that mentions the source binder cannot escape into the caller's domain"):
    val test = TField("x", TVar(a))
    val source = TUniv(a, TLam(TVar(a), TJointType(JointMode.Inter, arm(test), arm(TNeg(test)))))
    assert(uncoveredArrowTarget(source, TLam(TVar(d), int))(using caller).isEmpty)
