package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the set of variables that appear in the bounds of a constraining type. */
  def getConstrainedVars(): Set[TVar] =
    type_ match
      case TConstraining(base, bounds) =>
        val baseVars = base.getConstrainedVars()
        val boundsVars = bounds.map(_.var_)
        Set.concat(baseVars, boundsVars)
      case TUnion(left, right) =>
        val leftVars  = left.getConstrainedVars()
        val rightVars = right.getConstrainedVars()
        Set.concat(leftVars, rightVars)
      case TInter(left, right) =>
        val leftVars  = left.getConstrainedVars()
        val rightVars = right.getConstrainedVars()
        Set.concat(leftVars, rightVars)
      case _ => Set.empty
