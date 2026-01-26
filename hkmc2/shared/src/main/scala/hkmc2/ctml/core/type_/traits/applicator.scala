package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*

/** Trait that describes a function application on a type. */
trait TypeApplicator[T[+_], B[+_],  P]:
  /** Apply the transformation on a type with the given parameters. */
  def apply(type_ : Type, p: P)(using first: TypeApplicator[T, B, P] = this): T[Type]

  /** Apply the transformation on a subtyping constraint with the given parameters. */
  def apply(constraint: Constraint, p: P)(using first: TypeApplicator[T, B, P]): B[Constraint]
