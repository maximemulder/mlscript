package hkmc2.ctml.core.type_.traits

abstract class TypeChainApplicator[T[+_], P](val next: TypeApplicator[T, P]) extends TypeApplicator[T, P]
