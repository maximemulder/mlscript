package hkmc2.ctml.types

import hkmc2.ctml.util.*

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
  override def toString: String =
    this.show

object Direction:
  /** The two subtyping directions. */
  def both: List[Direction] = List(Direction.Sub, Direction.Super)

/** Implementation of the `Show` trait for `Direction`. */
implicit def DirectionShow: Show[Direction] = new Show {
  override def show(dir: Direction): String =
    dir match
      case Direction.Sub   => "≤"
      case Direction.Super => "≥"
}
