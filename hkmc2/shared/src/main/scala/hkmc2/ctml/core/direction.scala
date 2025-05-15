package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Combine two types according to a given type direction. */
def combine(left: Type, right: Type, dir: Direction)(using ctx: Context): Type =
  dir match
    case Direction.Sub   => meet(left, right)
    case Direction.Super => join(left, right)

/** Combineof many types according to a given type direction. */
def combineMany(types: List[Type], dir: Direction)(using ctx: Context): Type =
  val extremalType = getExtremalType(dir)
  types.foldRight(extremalType)(combine(_, _, dir))

/** Get the extremal type of a given type direction. */
def getExtremalType(dir: Direction): Type =
  dir match
    case Direction.Sub   => TTop
    case Direction.Super => TBot
