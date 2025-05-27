package hkmc2.ctml.core

import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

/** Attach some bounds to a constraining type. */
def attachConstrainingBounds(type_ : Type, bounds: List[Bound])(using ctx: Clauses): Type =
  val filteredBounds = ctx.filterUnsatisfiedBounds(bounds)
  if filteredBounds == Nil then
    type_
  else
    TConstraining(type_, bounds)

def attachConstrainedBounds(type_ : Type, varName: String, lowerBound: Type, upperBound: Type): Type =
  val bounds = ListBuffer[Bound]()
  if upperBound != TTop then
    bounds.append(Bound(varName, Direction.Sub, upperBound))

  if lowerBound != TBot then
    bounds.append(Bound(varName, Direction.Super, lowerBound))

  TConstrained(List(varName), type_, bounds.toList)
