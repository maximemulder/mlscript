package hkmc2.ctml.core.traverse

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Check whether the type is a constraining type. */
  def isConstraining(): Boolean =
    type_ match
      case TUnion(left, right) =>
        left.isConstraining() || right.isConstraining()
      case TInter(left, right) =>
        left.isConstraining() || right.isConstraining()
      case _ : TConstraining =>
        true
      case _ =>
        false
