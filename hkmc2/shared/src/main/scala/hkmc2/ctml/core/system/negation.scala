package hkmc2.ctml.core.system

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.isSubClass
import hkmc2.ctml.core.combine.join

extension (type_ : Type)
  /** Get the simplified negation of this type. */
  def negate(): Type =
    type_ match
      case TNeg(body) =>
        body.negateStep() match
          case Some(type_) =>
            type_.negate()
          case None =>
            type_
      case _ =>
        TNeg(type_)

  /** Evaluate a negation simplification step if possible. */
  def negateStep(): Option[Type] =
    type_ match
      case TBot =>
        Some(TTop)
      case TTop =>
        Some(TBot)
      case TNeg(body) =>
        Some(body)
      case _ =>
        None

  /** Subtract another type from this type. */
  def subtract(other: Type)(using ctx: Context, cache: SubtypingCache): Type =
    if checkSubtype(type_, other) then
      return TBot

    type_ match
      case TUnion(left, right) =>
        return join(left.subtract(other), right.subtract(other))
      case _ =>

    // If the types are disjoint,
    if areDisjointConstructors(type_, other) then
      return type_

    // Otherwise, return their intersection.
    TInter(type_, TNeg(other))
