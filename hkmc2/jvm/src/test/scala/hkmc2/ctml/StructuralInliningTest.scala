package hkmc2.ctml.core.simplification

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Preserve recursive structural obligations when eliminating an alias in a bound. */
class StructuralInliningTest extends AnyFunSuite with BeforeAndAfterEach:
  /** Keep optional solver settings independent of other tests. */
  override def beforeEach(): Unit =
    super.beforeEach()
    resetConfig()

  /** Restore the defaults after every probe. */
  override def afterEach(): Unit =
    try resetConfig()
    finally super.afterEach()

  /** The variable whose recursive obligation must survive substitution. */
  private val owner = TypeVar("Owner")

  /** An alias of the owner, substituted inside its bound. */
  private val alias = TypeVar("Alias")

  /** Both alias bounds agree, so its substitution is valid at either variance. */
  private def context: SubContext = SubContext.empty
    .declTypeVar(owner, TypeVarKind.Flex).declTypeVar(alias, TypeVarKind.Flex)
    .extend(Bound(alias, Direction.Sub, TVar(owner), BoundKind.Asserted))
    .extend(Bound(alias, Direction.Super, TVar(owner), BoundKind.Asserted))

  test("alias substitution preserves the owner beneath every structural position"):
    val shapes: List[Type => Type] = List(
      t => TField("tail", t), t => TTuple(t, TTop),
      t => TLam(TTop, t), t => TLam(t, TTop), t => TApp(TTop, t))
    for direction <- Direction.values; shape <- shapes do
      val bound = Bound(owner, direction, shape(TVar(alias)), BoundKind.Asserted)
      val substituted = bound.inline(alias)(using context)
      assert(substituted.type_ == shape(TVar(owner)),
        s"Lost structural obligation: $bound became $substituted")

  test("only a direct reflexive operand is removed after substituting a mixed bound"):
    for direction <- Direction.values do
      val mode = direction.jointMode
      val bound = Bound(owner, direction,
        TJointType(mode, TVar(alias), TField("tail", TVar(alias))), BoundKind.Asserted)
      val substituted = bound.inline(alias)(using context)
      assert(substituted.type_ == TField("tail", TVar(owner)),
        s"Structural self occurrence was erased with the direct one: $substituted")

  test("a direct alias relation remains a tautological self bound after elimination"):
    for direction <- Direction.values do
      val bound = Bound(owner, direction, TVar(alias), BoundKind.Asserted)
      val substituted = bound.inline(alias)(using context)
      val identity = if direction == Direction.Super then TBot else TTop
      assert(substituted.type_ == identity)
