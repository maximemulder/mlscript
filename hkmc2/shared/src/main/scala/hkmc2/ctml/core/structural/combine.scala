package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

/** Structurally combine two types while removing redundant top and bottom types. */
def structuralCombine(left: Type, right: Type, pol: Polarity) =
  pol match
    case Polarity.Negative =>
      (left, right) match
        case (TTop, TTop) =>
          TTop
        case (left, TTop) =>
          left
        case (TTop, right) =>
          right
        case (left, right) =>
          TInter(left, right)
    case Polarity.Positive =>
      (left, right) match
        case (TBot, TBot) =>
          TBot
        case (left, TBot) =>
          left
        case (TBot, right) =>
          right
        case (left, right) =>
          TUnion(left, right)
