package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*

extension (var_ : TypeVar)(using ctx: Context, mode: ConstraintMode)
  /** Check whether this type variable is rigid in the current context and constraining mode. */
  def isRigidMode: Boolean =
    mode match
      case ConstraintMode.Solve =>
        var_.isRigid
      case ConstraintMode.Reconstruct =>
        var_.isFlex

  /** Check whether this type variable is flexible in the current context and constraining mode. */
  def isFlexMode: Boolean =
    mode match
      case ConstraintMode.Solve =>
        var_.isFlex
      case ConstraintMode.Reconstruct =>
        var_.isRigid
