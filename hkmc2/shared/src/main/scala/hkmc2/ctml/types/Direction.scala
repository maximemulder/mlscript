package hkmc2.ctml.types

/** A subtyping direction. */
enum Direction:
  /** The subtype direction. */
  case Sub
  /** The supertype direction. */
  case Super

  /** Invert a subtyping direction. */
  def invert(): Direction =
    this match
      case Sub   => Super
      case Super => Sub

  /** Get the string representation of the object. */
  override def toString(): String =
    this.show()

object Direction:
  /** The two subtyping directions. */
  def both: List[Direction] = List(Direction.Sub, Direction.Super)
