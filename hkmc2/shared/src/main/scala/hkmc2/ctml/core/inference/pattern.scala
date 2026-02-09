package hkmc2.ctml.core.inference

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Check whether this type is an acceptable pattern or not. */
  def isPattern(using ctx: Context): Boolean =
    type_ match
      case TVar(var_) if var_.isClass =>
        true
      case TNeg(body) =>
        body.isPattern
      case TUnion(left, right) =>
        left.isPattern && right.isPattern
      case TInter(left, right) =>
        left.isPattern && right.isPattern
      case _ =>
        false
