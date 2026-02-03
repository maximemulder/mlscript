package hkmc2.ctml.core.system

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*

/** Get the normalized negation of a type. */
def negate(type_ : Type): Option[Type] =
  type_ match
    case TBot =>
      Some(TTop)
    case TTop =>
      Some(TBot)
    case TNeg(body) =>
      Some(body)
    case _ =>
      None

/** Normalize the negations in a type. */
def normalizeNegation(type_ : Type): Type =
  type_ match
    case TNeg(body) =>
      negate(body) match
        case Some(type_) =>
          normalizeNegation(type_)
        case None =>
          type_
    case _ =>
      type_

/** Check if two types are disjoint constructor types. */
def areDisjointConstructors(left: Type, right: Type)(using ctx: Context): Boolean =
  (left, right) match
    case (TLam(_, _), TLam(_, _)) =>
      false
    case (TTuple(_, _), TTuple(_, _)) =>
      false
    case (TVar(_), TVar(_)) if left.isClassVar && right.isClassVar && left == right =>
      false
    case _ =>
      isConstructor(left) && isConstructor(right)

/** Check if a type is a constructor type. */
def isConstructor(type_ : Type)(using ctx: Context): Boolean =
  type_ match
    case TVar(_) if type_.isClassVar =>
      true
    case TTuple(_, _) =>
      true
    case TLam(_, _) =>
      true
    case _ =>
      false
