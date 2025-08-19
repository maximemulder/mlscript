package hkmc2.ctml.core.type_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getVars()(using ctx: Context): Set[TypeVar] =
    type_ match
      case TVar(var_) if !var_.isClass =>
        Set(var_)
      case TUniv(var_, body) =>
        given Context = ctx.extend(declRigidVar(var_))
        body.getVars()
      case _ =>
        type_.accumulate(_.getVars())
