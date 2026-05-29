package hkmc2.ctml.types

import hkmc2.ctml.utils.*

/** A subtyping direction. */
enum Direction:
  /** The subtype direction. */
  case Sub
  /** The supertype direction. */
  case Super

  /** Invert a subtyping direction. */
  def unary_! : Direction =
    this match
      case Sub   => Super
      case Super => Sub

  /** The type polarity at the left of the subtyping direction. */
  def leftPol: Polarity =
    this match
      case Sub   => Polarity.Negative
      case Super => Polarity.Positive

  /** The type polarity at the right of the subtyping direction. */
  def rightPol: Polarity =
    this match
      case Sub   => Polarity.Positive
      case Super => Polarity.Negative

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

object Direction:
  /** The two subtyping directions. */
  def both: List[Direction] = List(Direction.Sub, Direction.Super)

/** Implementation of the `Show` trait for `Direction`. */
given Show[Direction] with
  override def show(dir: Direction): String =
    dir match
      case Direction.Sub   => "≤"
      case Direction.Super => "≥"
