package hkmc2.ctml.util

/** The monoid trait. */
trait Monoid[T]:
  /** The identity element. */
  def empty: T

  /** The associative operation. */
  def combine(a: T, b: T): T

/** Implementation of the `Monoid` trait for `Unit`. */
given Monoid[Unit] with
  def empty = ()
  def combine(a: Unit, b: Unit): Unit =
    ()

/** Implementation of the `Monoid` trait for `Set`. */
given [T]: Monoid[Set[T]] with
  def empty = Set.empty
  def combine(a: Set[T], b: Set[T]): Set[T] =
    a ++ b

/** Implementation of the `Monoid` trait for `List`. */
given [T]: Monoid[List[T]] with
  def empty = List.empty
  def combine(a: List[T], b: List[T]): List[T] =
    a ++ b

/** Implementation of the `Monoid` trait for the logical conjunction. */
def AnyMonoid: Monoid[Boolean] = new Monoid:
  def empty = false
  def combine(a: Boolean, b: Boolean): Boolean =
    a || b

/** Implementation of the `Monoid` trait for the logical disjunction. */
def AllMonoid: Monoid[Boolean] = new Monoid:
  def empty = false
  def combine(a: Boolean, b: Boolean): Boolean =
    a && b
