package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

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

  /** Attached some constrained bounds to the type. */
  def attachConstrainedBounds(var_ : TypeVar, lowerBound: Type, upperBound: Type): Type =
    var bounds: List[Bound] = Nil

    // Do not add the top upper bound or bottom lower bound as those are implicit.

    if upperBound != TTop then
      bounds ::= Bound(var_, Direction.Sub, upperBound)

    if lowerBound != TBot then
      bounds ::= Bound(var_, Direction.Super, lowerBound)

    // Do not make a constrained type if there are no bounds to satisfy.

    if bounds == Nil then
      return type_

    // If the type is a constrained type, add the constrained bounds directly on it.

    type_ match
      case TConstrained(body, constrainedBounds) =>
        TConstrained(body, bounds ::: constrainedBounds)
      case _ =>
        TConstrained(type_, bounds)

  /** Attach some constraining bounds to the type. */
  def attachConstrainingBounds(bounds: List[Bound])(using ctx: Context): Type =
    val filteredBounds = ctx.filterUnsatisfiedBounds(bounds)
    if filteredBounds == Nil then
      type_
    else
      TConstraining(type_, filteredBounds)
