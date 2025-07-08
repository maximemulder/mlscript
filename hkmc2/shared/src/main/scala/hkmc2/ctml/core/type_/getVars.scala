package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.util.given

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getVars(): Set[TypeVar] =
    type_ match
      case TVar(var_) =>
        Set(var_)
      case _ =>
        type_.accumulate(_.getVars())
