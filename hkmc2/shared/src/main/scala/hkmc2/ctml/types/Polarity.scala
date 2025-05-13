package hkmc2.ctml.types

/** A type polarity. */
enum Polarity:
  /** The negative type polarity. */
  case Negative
  /** The positive type polarity. */
  case Positive

  /** Invert a type polarity. */
  def invert(): Polarity =
    this match
      case Negative => Positive
      case Positive => Negative

/** The polarities at which a variable occurs in a type. */
case class Polarities(
  /** Does the variable occurs at a negative polarity? */
  val negative: Boolean,
  /** Does the variable occurs at a positive polarity? */
  val positive: Boolean,
)

object Polarities:
  import Polarity.*

  /** Empty polarity occurences, when a variable does not occur in a type. */
  def empty = Polarities(false, false)

  /** Get the polarity occurences from a single type polarity. */
  def fromPolarity(polarity: Polarity) =
    polarity match
      case Negative =>
        Polarities(true, false)
      case Positive =>
        Polarities(false, true)

  /** Join two polarity occurences. */
  def join(left: Polarities, right: Polarities): Polarities =
    val negative = left.negative || right.negative
    val positive = left.positive || right.positive
    return Polarities(negative, positive)

  /** Meet two polarity occurences. */
  def meet(left: Polarities, right: Polarities): Polarities =
    val negative = left.negative && right.negative
    val positive = left.positive && right.positive
    return Polarities(negative, positive)
