package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*

abstract class TypeChainApplicator[T[+_], B[+_], P](val next: TypeApplicator[T, B, P]) extends TypeApplicator[T, B, P]:
  def apply(type_ : Type, p: P)(using first: TypeApplicator[T, B, P]): T[Type] =
    next.apply(type_, p)

  def apply(constraint: Constraint, p: P)(using first: TypeApplicator[T, B, P]): B[Constraint] =
    next.apply(constraint, p)
