package hkmc2.ctml.core

import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

extension (bounds: List[Bound])
  /** Get the combination of the bounds of a given type variable in a given direction, or the
   *  extremal type of that direction if the variable is not bounded in these bounds. */
  def getVarDirType(var_ : TypeVar, dir: Direction): Type =
    bounds
      .filter(_.isTypeVarDirBound(var_, dir))
      .map(_.type_)
      .reduceRightOption(makeJointType(dir.jointMode, _, _))
      .getOrElse(getExtremalType(dir))

  /** Combine the bounds of each type variable in each direction into a single bound, which takes
   *  the place of the leftmost of these bounds. */
  def combineVarBounds(): List[Bound] =
    bounds
      .map(bound => (bound.var_, bound.dir))
      .distinct
      .map((var_, dir) => Bound(var_, dir, bounds.getVarDirType(var_, dir), BoundKind.Asserted))

  /** Remove the bounds of a variable from the list of bounds. */
  def removeVar(var_ : TypeVar): List[Bound] =
    bounds.filter(_.var_ != var_)

  /** Filter the variables bounded in a given direction in the context. */
  def filterBoundedVars(vars: List[TypeVar], dir: Direction): List[TypeVar] =
    vars.filter(bounds.isTypeVarBounded(_, dir))

  /** Check whether a type variable is constrained in a given direction. */
  def isTypeVarBounded(var_ : TypeVar, dir: Direction): Boolean =
    bounds.exists(_.isTypeVarDirBound(var_, dir))

extension (bound: Bound)
  /** Check whether the bound bounds a given type variable in a given direction. */
  def isTypeVarDirBound(var_ : TypeVar, dir: Direction): Boolean =
    bound.var_ == var_ && bound.dir == dir
