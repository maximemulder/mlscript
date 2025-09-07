package hkmc2.ctml.core.type_.impls

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.type_.traits.{TypeContextApplicator, WithContext}
import hkmc2.ctml.core.type_.traits.TypeDispatcher

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Context): Type =
    TypeSimplify(type_, TypeSimplifyParams(ctx))

/** Parameters of the type simplification operation. */
class TypeSimplifyParams(val ctx: Context) extends WithContext[TypeSimplifyParams]:
  def getContext = ctx
  def setContext(ctx: Context) = TypeSimplifyParams(ctx)

/** Implementation of the type simplification operation. */
object TypeSimplify extends TypeContextApplicator[Const[Type], TypeSimplifyParams](
  new TypeDispatcher[Const[Type], Id, TypeSimplifyParams](
    TypeSimplifyCombinator[TypeSimplifyParams]
  ):
    override def apply(bounds: List[Bound], p: TypeSimplifyParams): Id[List[Bound]] =
      bounds
)
