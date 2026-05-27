package hkmc2.ctml.core.combine

import hkmc2.ctml.types.*
import hkmc2.ctml.core.subtyping.SubtypingCache

/** Combine two types as a join or a meet according to a typing direction. */
def combine(left: Type, right: Type, dir: Direction)(using ctx: Context): Type =
  dir match
    case Direction.Sub   => meet(left, right)
    case Direction.Super => join(left, right)

extension (types: List[Type])
  /** Combine many types as a join or a meet according to a typing direction. */
  def combineMany(dir: Direction)(using ctx: Context): Type =
    dir match
      case Direction.Sub =>
        types.meetMany()
      case Direction.Super =>
        types.joinMany()
