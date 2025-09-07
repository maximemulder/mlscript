package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*

/** Trait that describes a function application on a type. */
trait TypeApplicator[T[+_], P]:
  /** Apply the transformation on a type with the given parameters. */
  def apply(type_ : Type, p: P)(using first: TypeApplicator[T, P] = this): T[Type]

/** Trait that describes a function application on some type variable bounds. */
trait BoundsApplicator[B[+_], P]:
  /** Apply the transformation on some type variable bounds with the given parameters. */
  def apply(bounds: List[Bound], p: P): B[List[Bound]]
