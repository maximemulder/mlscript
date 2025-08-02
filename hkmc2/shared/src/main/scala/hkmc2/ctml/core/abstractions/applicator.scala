package hkmc2.ctml.core.abstractions

import hkmc2.ctml.types.*

/** Apply a function on a type with the given parameters. */
trait TypeApplicator[T[+_], P]:
  def apply(type_ : Type, p: P): T[Type]

/** Apply a function on a type variable bound with the given parameters. */
trait BoundsApplicator[B[+_], P]:
  def apply(bounds: List[Bound], p: P): B[List[Bound]]
