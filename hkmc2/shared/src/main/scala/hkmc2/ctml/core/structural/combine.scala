package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

/** Structurally combine two types while removing redundant top and bottom types. */
def structuralCombine(mode: JointMode, left: Type, right: Type) =
  mode match
    case JointMode.Union =>
      (left, right) match
        case (TBot, TBot) =>
          TBot
        case (left, TBot) =>
          left
        case (TBot, right) =>
          right
        case (left, right) =>
          TJointType(mode, left, right)
    case JointMode.Inter =>
      (left, right) match
        case (TTop, TTop) =>
          TTop
        case (left, TTop) =>
          left
        case (TTop, right) =>
          right
        case (left, right) =>
          TJointType(mode, left, right)
