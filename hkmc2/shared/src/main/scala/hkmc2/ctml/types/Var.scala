package hkmc2.ctml.types

/** A type variable. */
case class TypeVar(val name: String):
  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
