package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.abstractions.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Replace a type variable by a substitute type in a type, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar, substitute: Type)(using ctx: Context): Type =
    TypeInline(type_, TypeInlineParams(var_, substitute, ctx))

/** Parameters of the type variable inlining operation. */
class TypeInlineParams(val var_ : TypeVar, val substitute: Type, val ctx: Context) extends WithContext[TypeInlineParams]:
  def getContext = ctx
  def setContext(ctx: Context) = TypeInlineParams(var_, substitute, ctx)

/** Implementation of the type variablie inlining operation. */
object TypeInline extends TypeContextDispatcher[Const[Type], TypeInlineParams](TypeSimplifyCombinator[TypeInlineParams]):
  override def apply(type_ : Type, params: TypeInlineParams): Type =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        params.substitute
      case _ =>
        super.apply(type_, params)

  override def apply(bounds: List[Bound], params: TypeInlineParams): List[Bound] =
    bounds.map(bound =>
      Bound(bound.var_, bound.dir, this.apply(bound.type_, params))
    )
