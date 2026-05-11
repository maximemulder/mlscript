package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (type_ : Type)
  /** Apply a fold on the direct components of a type. */
  def fold[T](acc: T)(f: (Type, T) => T) =
    type_.components.foldRight(acc)(f)

extension (type_ : Type)
  /** Accumulate a monoidic value on the direct components of a type. */
  def accumulate[T](f: Type => T)(using m: Monoid[T]): T =
    type_.fold(m.empty)((type_, acc) => m.combine(acc, f(type_)))
