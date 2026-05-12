package hkmc2.ctml.core.type_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.utils.given

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getVars()(using ctx: Context): Set[TypeVar] =
    type_ match
      case TVar(var_) =>
        Set(var_)
      case TUniv(var_, body) =>
        given Context = ctx.declVar(var_, TypeVarKind.Rigid)
        body.getVars() - var_
      case _ =>
        type_.accumulate(_.getVars())

extension (constraint: Constraint)
  /** Get the type variables referenced in a subtyping constraint. */
  def getVars()(using ctx: Context): Set[TypeVar] =
    constraint.left.getVars() ++ constraint.right.getVars()
