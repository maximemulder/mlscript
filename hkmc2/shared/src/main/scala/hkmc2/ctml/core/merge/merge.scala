package hkmc2.ctml.core.merge

import hkmc2.ctml.types.*

/** Get the extremal type of a typing direction. */
def getExtremalType(dir: Direction): Type =
  dir match
    case Direction.Sub   => TTop
    case Direction.Super => TBot

/** Merge two types as a join or meet according to a typing direction. */
def merge(left: Type, right: Type, dir: Direction)(using ctx: Clauses): Type =
  dir match
    case Direction.Sub   => meet(left, right)
    case Direction.Super => join(left, right)

extension (types: List[Type])
  /** Merge many types according to a typing direction. */
  def mergeMany(dir: Direction)(using ctx: Clauses): Type =
    dir match
      case Direction.Sub =>
        types.meetMany()
      case Direction.Super =>
        types.joinMany()
