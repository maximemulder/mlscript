package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

/** Attach some constraining bounds to a type. */
def attachConstrainingBounds(type_ : Type, bounds: List[Bound])(using ctx: Context): Type =
  val filteredBounds = ctx.filterUnsatisfiedBounds(bounds)
  if filteredBounds == Nil then
    type_
  else
    TConstraining(type_, filteredBounds)

/** Attached some constrained bounds to a type. */
def attachConstrainedBounds(type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type): Type =
  val boundsBuffer = ListBuffer[Bound]()
  if upperBound != TTop then
    boundsBuffer.append(Bound(var_, Direction.Sub, upperBound))

  if lowerBound != TBot then
    boundsBuffer.append(Bound(var_, Direction.Super, lowerBound))

  val bounds = boundsBuffer.toList

  type_ match
    case TConstrained(body, constrainedBounds) =>
      TConstrained(body, bounds ::: constrainedBounds)
    case _ =>
      if bounds == Nil then
        type_
      else
        TConstrained(type_, bounds)

extension (type_ : Type)
  /** Split the body type and constrained bounds of the type. */
  def splitConstrained(): (Type, List[Bound]) =
    type_ match
      case TConstrained(body, bounds) =>
        (body, bounds)
      case _ =>
        (type_, Nil)

  /** Split the body type and constraining bounds of the type. */
  def splitConstrainings(): (Type, List[Bound]) =
    type_ match
      case TConstraining(body, bounds) =>
        (body, bounds)
      case _ =>
        (type_, Nil)
