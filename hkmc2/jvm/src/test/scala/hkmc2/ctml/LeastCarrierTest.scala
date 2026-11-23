package hkmc2.ctml.core.subtyping

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.core.structural.getVars
import hkmc2.ctml.types.*

/** Checks witness synthesis independently of the fallback to ordinary instantiation. */
class LeastCarrierTest extends AnyFunSuite:
  test("a dense component leaves the witness for an independent cycle available"):
    val x = TypeVar("X")
    val y = TypeVar("Y")
    val dense = List.tabulate(4)(i => TypeVar(s"D$i"))

    /** Expose each coordinate in the result and in the rotated constructor arguments. */
    def tuple(vs: List[TypeVar]): Type = vs.map(TVar.apply).reduceLeft[Type](TTuple.apply)

    /** Apply a distinct constructor to each rotation of the same tuple. */
    val denseGuards = dense.zipWithIndex.map: (v, i) =>
      val args = tuple(dense.drop(i) ++ dense.take(i))
      Constraint(TApp(TClass(ClassVar(s"H$i")), args), Direction.Sub, TVar(v))
    val pairGuards = List(
      Constraint(TApp(TClass(ClassVar("F")), TVar(y)), Direction.Sub, TVar(x)),
      Constraint(TApp(TClass(ClassVar("G")), TVar(x)), Direction.Sub, TVar(y)))
    val body = TTuple(TTuple(TVar(x), TVar(y)), tuple(dense))
    for order <- List(dense ++ List(x, y), List(x, y) ++ dense) do
      val guarded = (denseGuards ++ pairGuards).foldRight(body: Type)((g, t) => TConstrained(t, g))
      val scheme = TUniv(order.head, order.tail.foldRight(guarded)(TUniv.apply))
      val result = leastCarrierInstance(scheme).getOrElse(fail("the independent cycle fits"))
      assert(result.getVars.isEmpty)
      val (remaining, _) = result.getUnivComponents
      assert(!remaining.contains(x) && !remaining.contains(y))
