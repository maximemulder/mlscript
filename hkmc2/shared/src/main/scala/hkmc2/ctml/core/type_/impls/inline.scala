package hkmc2.ctml.core.type_.impls.inline

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.getVarBound
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (type_ : Type)
  /** Replace a type variable by a substitute type in a type, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar)(using ctx: Context): Type =
    TypeInline1(type_, TypeInlineParams(var_, Polarity.Positive, ctx))

extension (bound: Bound)
  /** Replace a type variable by a substitute type in a bound, simplifying the resulting type if
      possible. */
  def inline(var_ : TypeVar)(using ctx: Context): Bound =
    Bound(
      bound.var_,
      bound.dir,
      TypeInline1(bound.type_, TypeInlineParams(var_, bound.dir.leftPol, ctx))
        .removeDirectVar(bound.var_, bound.dir.leftPol)
    )

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
object TypeInline1 extends TypeShadowApplicator[Const[Type], Const[Constraint], TypeInlineParams](TypeInline2):
  override def univ(univ: TUniv): Type =
    univ

private def TypeInline2 = TypeContextApplicator[Const[Type], TypeInlineParams](TypeInline3, Combinator)

private def TypeInline3 = TypePolarityApplicator[Const[Type], Const[Constraint], TypeInlineParams](TypeInline4, Combinator)

private object TypeInline4 extends TypeChainApplicator[Const[Type], Const[Constraint], TypeInlineParams](TypeInline5):
  override def apply(type_ : Type, params: TypeInlineParams)(using first: TypeApplicator[Const[Type], Const[Constraint], TypeInlineParams]): Type =
    type_ match
      case TVar(var_) if var_ == params.var_ =>
        params.ctx.getVarBound(var_, params.pol.dir)
      case _ =>
        next.apply(type_, params)

private def TypeInline5 = TypeLazyDispatcher[TypeInlineParams](Combinator)

private def Combinator = TypeSimplifyCombinator[TypeInlineParams]
