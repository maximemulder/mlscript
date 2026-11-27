package hkmc2.ctml.core.subtyping

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.var_.{declClass, declTypeVar}
import hkmc2.ctml.types.*

/** Replay both assignments after the solver returns conditional caller bounds. */
class SourceGuardTest extends AnyFunSuite:
  test("a guarded conjunct and an empty field preserve alternative caller bounds"):
    val p = TypeVar("P")
    val q = TypeVar("Q")
    val int = TClass(ClassVar("Int"))
    val str = TClass(ClassVar("Str"))
    given ctx: SubContext = SubContext.empty.declClass("Int", None).declClass("Str", None)
      .declTypeVar(p, TypeVarKind.Flex).declTypeVar(q, TypeVarKind.Flex)

    val guarded = TConstrained(TBot, Constraint(TVar(p), Direction.Sub, int))
    val field = TField("x", TVar(q))
    val target = TJointType(JointMode.Union, TField("x", int), str)

    /** Fix the variable in both directions in the context returned by the previous step. */
    def fix(start: SubContext, variable: TypeVar, value: Type): SubContext =
      val lower = start.extend(subtype(value, TVar(variable))(using start))
      lower.extend(subtype(TVar(variable), value)(using lower))

    for (source, wanted, other) <- List(
      (TJointType(JointMode.Inter, guarded, field), target, int),
      (TJointType(JointMode.Inter, field, guarded), target, int),
      (field, TNeg(guarded), TBot),
    ) do
      val solved = ctx.extend(subtype(source, wanted))
      for pv <- List(int, str); qv <- List(other, str) do
        withClue(s"$source ≤ $wanted, P = $pv, Q = $qv: "):
          if pv == int || qv == other then fix(fix(solved, p, pv), q, qv)
          else intercept[TypeError](fix(fix(solved, p, pv), q, qv))
