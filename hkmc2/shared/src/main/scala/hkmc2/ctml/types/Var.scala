package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A type variable. */
case class TypeVar(val name: String):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `TypeVar`. */
given Show[TypeVar] with
  override def show(var_ : TypeVar): String =
    var_.name
