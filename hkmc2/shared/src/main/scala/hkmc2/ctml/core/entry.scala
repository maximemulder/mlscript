package hkmc2.ctml.core

import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

extension (clauses: Clauses)
  def getVarBounds(var_ : TypeVar): (List[Type], List[Type]) =
    val lowerBounds: ListBuffer[Type] = new ListBuffer()
    val upperBounds: ListBuffer[Type] = new ListBuffer()

    for bound <- clauses.varBounds(var_.name) do
      bound.dir match
        case Direction.Sub =>
          upperBounds.append(bound.type_)
        case Direction.Super =>
          lowerBounds.append(bound.type_)

    (lowerBounds.toList, upperBounds.toList)


/** Remove the variables in the context that appear before a certain level. */
def removeLowVars(ctx: Clauses, vars: List[TypeVar]): List[TypeVar] =
  // TODO
  vars
