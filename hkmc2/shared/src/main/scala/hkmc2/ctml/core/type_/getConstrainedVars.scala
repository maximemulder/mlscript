package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the set of variables that appear in the bounds of a constraining type. */
  def getConstrainedVars(): Set[TypeVar] =
    type_ match
      case TConstraining(body, constraint) =>
        Set.concat(
          body.getConstrainedVars(),
          constraint.left.getConstrainedVars(),
          constraint.right.getConstrainedVars(),
        )
      case TUnion(left, right) =>
        Set.concat(
          left.getConstrainedVars(),
          right.getConstrainedVars(),
        )
      case TInter(left, right) =>
        Set.concat(
          left.getConstrainedVars(),
          right.getConstrainedVars(),
        )
      case _ =>
        Set.empty
