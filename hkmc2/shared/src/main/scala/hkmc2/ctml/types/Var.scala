package hkmc2.ctml.types

import hkmc2.ctml.utils.*

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

/** How to treat a type variable during type inference level solving. */
enum VarAction:
  /** Quantify the type variable. */
  case Quantify
  /** Skip the type variable. */
  case Skip
  /** Inline the type variable. */
  case Inline

  /** Get the string representation of the object. */
  override def toString(): String =
    this match
      case Quantify =>
        "quantify"
      case Skip =>
        "skip"
      case Inline =>
        "inline"
