package hkmc2.ctml.core.system

import hkmc2.ctml.types.*

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
