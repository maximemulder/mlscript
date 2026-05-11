package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** The subtyping refinement mode. */
enum RefineMode:
  /** The context may not be refined to solve a subtyping constraint. */
  case Check
  /** The context may be refined to solve a subtyping constraint. */
  case Constrain

  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `RefineMode`. */
given Show[RefineMode] with
  override def show(mode: RefineMode): String =
    mode match
      case RefineMode.Constrain => "constrain"
      case RefineMode.Check     => "check"

/** The subtyping constraint mode. */
enum ConstraintMode:
  /** Solve the subtyping constraint in the context. */
  case Solve
  /** Propagate the subtyping information in the context without solving it. */
  case Reconstruct

  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `ConstraintMode`. */
given Show[ConstraintMode] with
  override def show(mode: ConstraintMode): String =
    mode match
      case ConstraintMode.Solve       => "solve"
      case ConstraintMode.Reconstruct => "reconstruct"
