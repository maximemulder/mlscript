package hkmc2.ctml.core.var_

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

def extrude(type_ : Type, level: TypeVar)(using ctx: Context): Type =
  type_ match
    case TVar(var_) if ctx.compareVarLevels(var_, level) == Order.GT =>
      type_
    case _ =>
      type_.map(extrude(_, level))
