package hkmc2.ctml.core

import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

extension (bounds: List[Bound])
  /** Get the type of the leftmost bound of a given type variable in a given direction, or the
   *  extremal type of that direction if the variable is not bounded in these bounds. */
  def getVarDirType(var_ : TypeVar, dir: Direction): Type =
    bounds.find(bound => bound.var_ == var_ && bound.dir == dir) match
      case Some(bound) =>
        bound.type_
      case None =>
        getExtremalType(dir)

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
