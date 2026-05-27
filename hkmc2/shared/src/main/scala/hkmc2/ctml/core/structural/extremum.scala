package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

/** Get the extremal type of a typing direction. */
def getExtremalType(dir: Direction): Type =
  dir match
    case Direction.Sub   => TTop
    case Direction.Super => TBot
