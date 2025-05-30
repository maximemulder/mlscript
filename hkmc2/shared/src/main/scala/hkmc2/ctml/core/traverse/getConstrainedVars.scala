package hkmc2.ctml.core.traverse

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the set of variables that appear in the bounds of a constraining type. */
  def getConstrainedVars(): Set[String] =
    type_ match
      case TConstraining(base, bounds) =>
        val baseVars = base.getConstrainedVars()
        val boundsVars = bounds.map(_.name)
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
