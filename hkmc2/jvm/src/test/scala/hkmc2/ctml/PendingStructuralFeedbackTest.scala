package hkmc2.ctml

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Recursive structural propagation must check each new bound against its pending opposite. */
class PendingStructuralFeedbackTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Reset the configuration and fresh-variable counter for each test. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()
    resetFreshVarCounter()
    config.searchSteps = Some(5000)

  /** Restore ordinary policy for later tests. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** The carrier shared by both sides of the feedback constraint. */
  private val x = TypeVar("X")

  /** An inhabited seed disjoint from the recursive constructor. */
  private val nilType: Type = TClass(ClassVar("LNil"))

  /** A constructor whose required tail is empty exactly when that field type is empty. */
  private def cell(tail: Type): Type =
    TJointType(JointMode.Inter, TClass(ClassVar("LCons")), TField("tail", tail))

  /** A seeded positive functor used without algebraic recursive closure. */
  private def layer(tail: Type): Type = TJointType(JointMode.Union, nilType, cell(tail))

  /** Plain flexible inference context without additional recursive rules. */
  private def context: SubContext = SubContext.empty
    .declClass("LNil", None).declClass("LCons", None).declClass("Int", None)
    .declTypeVar(x, TypeVarKind.Flex)

  test("an incoming seed upper bound cannot hide a newly propagated empty recursive tail"):
    val upper = subtype(TVar(x), nilType)(using context)
    intercept[TypeError](subtypeSeq(layer(TVar(x)), TVar(x), upper)(using context))

  test("the seeded contradiction rejects in the reverse constraint order"):
    val lower = subtype(layer(TVar(x)), TVar(x))(using context)
    intercept[TypeError](subtypeSeq(TVar(x), nilType, lower)(using context))

  test("the complement dual preserves the same contradictory feedback"):
    val lower = subtype(TNeg(nilType), TVar(x))(using context)
    intercept[TypeError](subtypeSeq(TVar(x), TNeg(layer(TNeg(TVar(x)))), lower)(using context))

  test("unseeded structural feedback retains its legitimate empty solution"):
    val upper = subtype(TVar(x), nilType)(using context)
    val result = subtypeSeq(cell(TVar(x)), TVar(x), upper)(using context)
    subtypeSeq(TVar(x), TBot, result)(using context)
    intercept[TypeError](subtypeSeq(nilType, TVar(x), result)(using context))

  test("a seeded producer retains its lower obligation under an unrestricted upper bound"):
    val result = subtype(layer(TVar(x)), TVar(x))(using context)
    subtypeSeq(TVar(x), TTop, result)(using context)
    subtypeSeq(nilType, TVar(x), result)(using context)
    intercept[TypeError](subtypeSeq(TVar(x), TClass(ClassVar("Int")), result)(using context))
