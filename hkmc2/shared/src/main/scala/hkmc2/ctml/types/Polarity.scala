package hkmc2.ctml.types

import hkmc2.ctml.util.*

/** A type polarity. */
enum Polarity:
  /** The negative type polarity. */
  case Negative
  /** The positive type polarity. */
  case Positive

  /** Invert a type polarity. */
  def invert: Polarity =
    this match
      case Negative => Positive
      case Positive => Negative

  /** The subtyping direction relevant to the type polarity. */
  def dir: Direction =
    this match
      case Negative => Direction.Sub
      case Positive => Direction.Super

  /** Get the product of this polarity with another polarity. */
  def product(other: Polarity): Polarity =
    other match
      case Negative => this.invert
      case Positive => this

  /** Get the string representation of the object. */
  override def toString: String =
    this.show

/** The type polarities at which a variable occurs in a type. */
case class Polarities(
  /** Does the variable occurs at a negative polarity? */
  val negative: Boolean,
  /** Does the variable occurs at a positive polarity? */
  val positive: Boolean,
)

object Polarities:
  import Polarity.*

  /** Empty type polarity occurences, when a variable does not occur in a type. */
  def empty = Polarities(false, false)

  /** Full type polarity occurences, when a variable appears in both polarities in a type. */
  def full = Polarities(true, true)

  /** Get the type polarity occurences from a single type polarity. */
  def fromPolarity(polarity: Polarity) =
    polarity match
      case Negative =>
        Polarities(true, false)
      case Positive =>
        Polarities(false, true)

  /** Join two type polarity occurences. */
  def join(left: Polarities, right: Polarities): Polarities =
    val negative = left.negative || right.negative
    val positive = left.positive || right.positive
    return Polarities(negative, positive)

  /** Meet two type polarity occurences. */
  def meet(left: Polarities, right: Polarities): Polarities =
    val negative = left.negative && right.negative
    val positive = left.positive && right.positive
    return Polarities(negative, positive)

/** Implementation of the `Show` trait for `Polarity`. */
given Show[Polarity] with
  override def show(pol: Polarity): String =
    pol match
      case Polarity.Negative => "−"
      case Polarity.Positive => "+"

/** Implementation of the `Semigroup` trait for the polarities join. */
def JoinPolaritiesSemigroup: Semigroup[Polarities] = new Semigroup:
  def combine(a: Polarities, b: Polarities): Polarities =
    Polarities.join(a, b)

/** Implementation of the `Semigroup` trait for the polarities meet. */
def MeetPolaritiesSemigroup: Semigroup[Polarities] = new Semigroup:
  def combine(a: Polarities, b: Polarities): Polarities =
    Polarities.meet(a, b)

/** Implementation of the `Monoid` trait for the polarities join. */
def JoinPolaritiesMonoid: Monoid[Polarities] = new Monoid(using JoinPolaritiesSemigroup):
  def empty = Polarities.empty

/** Implementation of the `Monoid` trait for the polarities meet. */
def MeetPolaritiesMonoid: Monoid[Polarities] = new Monoid(using MeetPolaritiesSemigroup):
  def empty = Polarities.full
