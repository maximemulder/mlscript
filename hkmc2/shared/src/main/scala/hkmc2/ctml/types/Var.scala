package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** A term variable. Resolved variables retain their elaborator identity; synthetic references
 *  select a prelude operation by name. */
case class TermVar(name: String, id: Option[Int]):
  /** Display the source name without its internal identity. */
  override def toString: String = name

/** A type variable. */
case class TypeVar(val name: String):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** A class variable. */
case class ClassVar(val name: String):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `TypeVar`. */
given Show[TypeVar] with
  override def show(var_ : TypeVar): String =
    var_.name

/** Implementation of the `Show` trait for `ClassVar`. */
given Show[ClassVar] with
  override def show(var_ : ClassVar): String =
    var_.name
