package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*

extension (var_ : TypeVar)(using ctx: SubContext, mode: ConstraintMode)
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

extension (type_ : Type)(using ctx: SubContext, mode: ConstraintMode)
  /** Check whether this type is a type variable that is flexible in the current context and
   *  constraining mode. */
  def isFlexModeVar: Boolean =
    type_ match
      case TVar(var_) =>
        var_.isFlexMode
      case _ =>
        false
