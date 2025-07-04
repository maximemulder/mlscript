package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Accumulate a monoidic value while traversing a type. */
  def accumulate[T](f: Type => Option[T])(using m: Monoid[T]): T =
    f(type_) match
      case Some(value) =>
        value
      case None =>
        type_.components.foldRight(m.empty)((type_, accumulator) =>
          val value = type_.accumulate(f)
          m.combine(accumulator, value)
        )

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getVars(): Set[TypeVar] =
    type_.accumulate(_ match
      case TVar(var_) =>
        Some(Set(var_))
      case _ =>
        None
    )
