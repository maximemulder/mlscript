package hkmc2.ctml.core.type_.impls.inline

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.core.context.getVarBound
import hkmc2.ctml.core.debug.output

extension (type_ : Type)
  /** Replace a type variable by a substitute type in a type, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar)(using ctx: Context): Type =
    TypeInline1(type_, TypeInlineParams(var_, Polarity.Positive, ctx))

/** Parameters of the type variable inlining operation. */
class TypeInlineParams(
  val var_ : TypeVar,
  val pol: Polarity,
  val ctx: Context,
) extends ContextParams[TypeInlineParams], PolarityParams[TypeInlineParams], TypeVarParams[TypeInlineParams]:
  override def setContext(ctx: Context) = TypeInlineParams(var_, pol, ctx)
  override def setPolarity(pol: Polarity) = TypeInlineParams(var_, pol, ctx)
  override def setVar(var_ : TypeVar) = TypeInlineParams(var_, pol, ctx)

/** Implementation of the type variable inlining operation. */
object TypeInline1 extends TypeShadowApplicator[Const[Type], TypeInlineParams](TypeInline2):
  override def univ(univ: TUniv): Type =
    univ

private def TypeInline2 = TypeContextApplicator[Const[Type], TypeInlineParams](TypeInline3, Combinator)

private def TypeInline3 = TypePolarityApplicator[Const[Type], Id, TypeInlineParams](TypeInline4, Combinator)

private object TypeInline4 extends TypeDispatcher[Const[Type], Id, TypeInlineParams](Combinator):
  override def apply(type_ : Type, params: TypeInlineParams)(using first: TypeApplicator[Const[Type], TypeInlineParams]): Type =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        params.ctx.getVarBound(var_, params.pol.dir)
      case _ =>
        super.apply(type_, params)

  override def apply(bound: Bound, params: TypeInlineParams): Bound =
    Bound(bound.var_, bound.dir, this.apply(bound.type_, params))

private def Combinator = TypeSimplifyCombinator[TypeInlineParams]
