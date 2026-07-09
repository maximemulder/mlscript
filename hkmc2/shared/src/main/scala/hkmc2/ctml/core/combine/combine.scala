package hkmc2.ctml.core.combine

import hkmc2.ctml.types.*
import hkmc2.ctml.core.subtyping.SubtypingCache

/** Combine two types as a join or a meet according to a joint mode. */
def combine(mode: JointMode, left: Type, right: Type)(using ctx: SubContext): Type =
  mode match
    case JointMode.Union =>
      join(left, right)
    case JointMode.Inter =>
      meet(left, right)

extension (types: List[Type])
  /** Combine many types as a join or a meet according to a joint mode. */
  def combineMany(mode: JointMode)(using ctx: SubContext): Type =
    mode match
      case JointMode.Union =>
        types.joinMany()
      case JointMode.Inter =>
        types.meetMany()
