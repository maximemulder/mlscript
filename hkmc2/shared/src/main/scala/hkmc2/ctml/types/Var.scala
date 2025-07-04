package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A type variable. */
case class TypeVar(val name: String):
  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** Implementation of the `Show` trait for `TypeVar`. */
implicit def TypeVarShow: Show[TypeVar] = new Show {
  override def show(var_ : TypeVar): String =
    var_.name
}

/** Convert a list of type variables to its string representation. */
def showTypeVars(vars: List[TypeVar]): String =
  vars.map(_.show).mkString(", ")
