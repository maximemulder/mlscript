package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (bounds: List[Bound])
  /** Remove the bounds of a variable from the list of bounds. */
  def removeVar(var_ : TVar): List[Bound] =
    bounds.filter(_.var_ != var_)

  /** Filter the variables bounded in a given direction in the context. */
  def filterBoundedVars(vars: List[TVar], dir: Direction): List[TVar] =
    vars.filter(bounds.isTypeVarBounded(_, dir))

  /** Check whether a type variable is constrained in a given direction. */
  def isTypeVarBounded(var_ : TVar, dir: Direction): Boolean =
    bounds.exists((bound) => bound.var_ == var_ && bound.dir == dir)

  def filterVarDir(var_ : TVar, dir: Direction): List[Type] =
    bounds
      .filter((bound) => bound.var_ == var_ && bound.dir == dir)
      .map(_.type_)
