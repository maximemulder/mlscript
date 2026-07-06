package hkmc2.ctml.core.structural

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  def structuralInline(var_ : TypeVar)(using ctx: Context): Type =
    type_.structuralInline(var_, Polarity.Positive)

  def structuralInline(var_ : TypeVar, pol: Polarity)(using ctx: Context): Type =
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        var_.bound(pol.dir)
      case TNeg(body) =>
        TNeg(
          body.structuralInline(var_, !pol)
        )
      case TTuple(left, right) =>
        TTuple(
          left.structuralInline(var_, pol),
          right.structuralInline(var_, pol),
        )
      case TLam(param, ret) =>
        TLam(
          param.structuralInline(var_, !pol),
          ret.structuralInline(var_, pol),
        )
      case TUnion(left, right) =>
        structuralCombine(
          JointMode.Union,
          left.structuralInline(var_, pol),
          right.structuralInline(var_, pol),
        )
      case TInter(left, right) =>
        structuralCombine(
          JointMode.Inter,
          left.structuralInline(var_, pol),
          right.structuralInline(var_, pol),
        )
      case TApp(abs, arg) =>
        TApp(
          abs.structuralInline(var_, pol),
          arg.structuralInline(var_, pol),
        )
      case TUniv(typeVar, body) if typeVar != var_ =>
        TUniv(
          typeVar,
          body.structuralInline(var_, pol),
        )
      case TConstrained(body, constraint) =>
        TConstrained(
          body.structuralInline(var_, pol),
          constraint.structuralInline(var_)(using ctx, !pol),
        )
      case TConstraining(body, constraint) =>
        TConstraining(
          body.structuralInline(var_, pol),
          constraint.structuralInline(var_)(using ctx, !pol),
        )
      case TBot | TTop | TVar(_) | TClass(_) | TUniv(_, _) =>
        type_

extension (constraint: Constraint)
  def structuralInline(var_ : TypeVar)(using ctx: Context, pol: Polarity): Constraint =
    Constraint(
      constraint.left.structuralInline(var_, !pol),
      constraint.dir,
      constraint.right.structuralInline(var_, pol),
    )

extension (bound: Bound)
  def structuralInline(var_ : TypeVar)(using ctx: Context): Bound =
    val newUpper = var_.upperBound.removeDirectVar(bound.var_, Polarity.Negative)
    val newLower = var_.lowerBound.removeDirectVar(bound.var_, Polarity.Positive)
    val newBoundType = bound.type_.structuralInline(var_, bound.dir.leftPol)(using
      ctx.extend(
        Bound(var_, Direction.Sub, newUpper),
        Bound(var_, Direction.Super, newLower),
      )
    ).removeDirectVar(bound.var_, bound.dir.leftPol)

    Bound(bound.var_, bound.dir, newBoundType)
