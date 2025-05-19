package hkmc2.ctml.types

/** A subtyping direction. */
enum Direction:
  /** The subtype direction. */
  case Sub
  /** The supertype direction. */
  case Super

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()
