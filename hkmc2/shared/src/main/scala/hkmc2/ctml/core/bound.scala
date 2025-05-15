package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (bounds: List[Bound])
  /** Remove the bounds of a variable from the list of bounds. */
  def removeVar(varName: String): List[Bound] =
    bounds.filter((bound) => bound.name != varName)

  /** Filter the variables bounded in a given direction in the context. */
  def filterBoundedVars(varNames: List[String], dir: Direction): List[String] =
    varNames.filter(bounds.isTypeVarBounded(_, dir))

  /** Check whether a type variable is constrained in a given direction. */
  def isTypeVarBounded(varName: String, dir: Direction): Boolean =
    bounds.exists((bound) => bound.name == varName && bound.dir == dir)

  def collectVarBounds(varName: String, dir: Direction): List[Type] =
    bounds
      .filter((bound) => bound.name == varName && bound.dir == dir)
      .map(_.type_)
