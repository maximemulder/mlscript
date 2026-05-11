package hkmc2.ctml.types

import hkmc2.ctml.utils.*
import hkmc2.ctml.utils.given

/** A subtyping constraint. */
class Constraint(val left: Type, val dir: Direction, val right: Type):
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `Constraint`. */
given Show[Constraint] with
  override def show(constraint: Constraint): String =
    s"${constraint.left} ${constraint.dir} ${constraint.right}"
