package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (bounds: List[Bound])
  /** Remove the bounds of a variable from the list of bounds. */
  def removeVar(var_ : TypeVar): List[Bound] =
    bounds.filter(_.var_ != var_)

  /** Filter the variables bounded in a given direction in the context. */
  def filterBoundedVars(vars: List[TypeVar], dir: Direction): List[TypeVar] =
    vars.filter(bounds.isTypeVarBounded(_, dir))

  /** Check whether a type variable is constrained in a given direction. */
  def isTypeVarBounded(var_ : TypeVar, dir: Direction): Boolean =
    bounds.exists((bound) => bound.var_ == var_ && bound.dir == dir)

  def filterVarDir(var_ : TypeVar, dir: Direction): List[Type] =
    bounds
      .filter((bound) => bound.var_ == var_ && bound.dir == dir)
      .map(_.type_)
