package hkmc2.ctml.util

/** The semigroup trait. */
trait Semigroup[T]:
  /** The associative operation. */
  def combine(a: T, b: T): T

/** Implementation of the `Semigroup` trait for `Unit`. */
given Semigroup[Unit] with
  def combine(a: Unit, b: Unit): Unit =
    ()

/** Implementation of the `Semigroup` trait for `Set`. */
given [T]: Semigroup[Set[T]] with
  def combine(a: Set[T], b: Set[T]): Set[T] =
    a ++ b

/** Implementation of the `Semigroup` trait for `List`. */
given [T]: Semigroup[List[T]] with
  def combine(a: List[T], b: List[T]): List[T] =
    a ++ b

/** Implementation of the `Semigroup` trait for the logical conjunction. */
def AnySemigroup: Semigroup[Boolean] = new Semigroup:
  def combine(a: Boolean, b: Boolean): Boolean =
    a || b

/** Implementation of the `Semigroup` trait for the logical disjunction. */
def AllSemigroup: Semigroup[Boolean] = new Semigroup:
  def combine(a: Boolean, b: Boolean): Boolean =
    a && b
