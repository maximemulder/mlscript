package hkmc2.ctml.core.type_.impls.inline

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.core.var_.removeDirectVar
import hkmc2.ctml.core.context.getVarBound

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
  val boundedVar: Option[TypeVar] = None,
) extends ContextParams[TypeInlineParams], PolarityParams[TypeInlineParams], TypeVarParams[TypeInlineParams]:
  override def setContext(ctx: Context) = TypeInlineParams(var_, pol, ctx, boundedVar)
  override def setPolarity(pol: Polarity) = TypeInlineParams(var_, pol, ctx, boundedVar)
  override def setVar(var_ : TypeVar) = TypeInlineParams(var_, pol, ctx, boundedVar)

/** Implementation of the type variable inlining operation. */
object TypeInline1 extends TypeShadowApplicator[Const[Type], TypeInlineParams](TypeInline2):
  override def univ(univ: TUniv): Type =
    univ

private def TypeInline2 = TypeContextApplicator[Const[Type], TypeInlineParams](TypeInline3, Combinator)

private def TypeInline3 = TypePolarityApplicator[Const[Type], Id, TypeInlineParams](TypeInline4, Combinator)

object TypeInline4 extends TypeLazyDispatcher(Combinator):
  override def apply(type_ : Type, params: TypeInlineParams)(using first: TypeApplicator[Const[Type], TypeInlineParams]): Type =
  type_ match
    case TVar(var_) if var_ == params.var_ =>
      val bound = params.ctx.getVarBound(var_, params.pol.dir)
      params.boundedVar match
        case Some(boundedVar) =>
          bound.removeDirectVar(boundedVar, params.pol)
        case None =>
          bound
    case TUnion(left, right) if params.pol == Polarity.Positive =>
      super.apply(type_, params)
    case TInter(left, right) if params.pol == Polarity.Negative =>
      super.apply(type_, params)
    case _ =>
      super.apply(type_, TypeInlineParams(params.var_, params.pol, params.ctx, None))

  override def apply(constraint: Constraint, params: TypeInlineParams): Constraint =
    Constraint(
      this.apply(constraint.left, params),
      constraint.dir,
      this.apply(constraint.right, params),
    )

private def Combinator = TypeSimplifyCombinator[TypeInlineParams]
