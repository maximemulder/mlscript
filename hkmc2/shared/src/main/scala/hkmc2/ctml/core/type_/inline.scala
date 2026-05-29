package hkmc2.ctml.core.type_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*
import hkmc2.ctml.config.debug

extension (type_ : Type)
  def inlineB(var_ : TypeVar)(using ctx: Context, pol: Polarity = Polarity.Positive): Type =
    type_ match
      case TVar(typeVar) if typeVar == var_ =>
        var_.bound(pol.dir)
      case TNeg(body) =>
        TNeg(
          body.inlineB(var_)
        )
      case TTuple(left, right) =>
        TTuple(
          left.inlineB(var_),
          right.inlineB(var_),
        )
      case TLam(param, ret) =>
        TLam(
          param.inlineB(var_)(using ctx, !pol),
          ret.inlineB(var_),
        )
      case TUnion(left, right) =>
        TUnion(
          left.inlineB(var_),
          right.inlineB(var_),
        )
      case TInter(left, right) =>
        TInter(
          left.inlineB(var_),
          right.inlineB(var_),
        )
      case TApp(abs, arg) =>
        TApp(
          abs.inlineB(var_),
          arg.inlineB(var_),
        )
      case TUniv(typeVar, body) if typeVar != var_ =>
        TUniv(
          typeVar,
          body.inlineB(var_),
        )
      case TConstrained(body, constraint) =>
        TConstrained(
          body.inlineB(var_),
          constraint.inlineB(var_)(using ctx, !pol),
        )
      case TConstraining(body, constraint) =>
        TConstraining(
          body.inlineB(var_),
          constraint.inlineB(var_)(using ctx, !pol),
        )
      case TBot | TTop | TVar(_) | TClass(_) | TUniv(_, _) =>
        type_

extension (constraint: Constraint)
  def inlineB(var_ : TypeVar)(using ctx: Context, pol: Polarity): Constraint =
    Constraint(
      constraint.left.inlineB(var_)(using ctx, !pol),
      constraint.dir,
      constraint.right.inlineB(var_),
    )

extension (bound: Bound)
  def inlineB(var_ : TypeVar)(using ctx: Context): Bound =
    val newBoundType = bound.type_.inlineB(var_)(using
      ctx.extend(
        Bound(var_, Direction.Sub,   var_.upperBound),
        Bound(var_, Direction.Super, var_.lowerBound),
      ),
      bound.dir.leftPol,
    ).removeDirectVar(bound.var_, bound.dir.leftPol)

    Bound(bound.var_, bound.dir, newBoundType)
