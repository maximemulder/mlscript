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

/** Make a constraining type from its components, simplifying it if possible. */
def makeConstrainingType(type_ : Type, bounds: List[Bound])(using ctx: Context): Type =
  ctx.filterUnsatisfiedBounds(bounds) match
    case Nil =>
      type_
    case filteredBounds =>
      TConstraining(type_, filteredBounds)

/** Make a lambda type from its components, simplifying it if possible. */
def makeLambdaType(param: Type, ret: Type): Type =
  ret match
    case TUniv(var_, body) =>
      val type_ = makeLambdaType(param, body)
      TUniv(var_, type_)
    case _ =>
      TLam(param, ret)
