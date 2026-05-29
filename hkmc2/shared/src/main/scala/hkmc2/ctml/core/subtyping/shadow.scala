package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  def shadow(using ctx: Context): Type =
    type_ match
      case TBot =>
        TBot
      case TTop =>
        TTop
      case TNeg(body) =>
        TNeg(body.shadow)
      case TVar(var_) =>
        TVar(var_.shadow)
      case TClass(var_) =>
        TClass(var_)
      case TTuple(left, right) =>
        TTuple(
          left.shadow,
          right.shadow,
        )
      case TLam(param, ret) =>
        TLam(
          param.shadow,
          ret.shadow,
        )
      case TUnion(left, right) =>
        TUnion(
          left.shadow,
          right.shadow,
        )
      case TInter(left, right) =>
        TInter(
          left.shadow,
          right.shadow,
        )
      case TApp(abs, arg) =>
        TApp(
          abs.shadow,
          arg.shadow,
        )
      case TUniv(var_, body) =>
        TUniv(
          var_,
          body.shadow,
        )
      case TConstrained(body, constraint) =>
        TConstrained(
          body.shadow,
          constraint.shadow,
        )
      case TConstraining(body, constraint) =>
        TConstraining(
          body.shadow,
          constraint.shadow,
        )

extension (var_ : TypeVar)
  def shadow(using ctx: Context): TypeVar =
    var_.origin match
      case Some(origin) =>
        origin.shadow
      case None =>
        var_

extension (constraint: Constraint)
  def shadow(using ctx: Context): Constraint =
    Constraint(
      constraint.left.shadow,
      constraint.dir,
      constraint.right.shadow,
    )
