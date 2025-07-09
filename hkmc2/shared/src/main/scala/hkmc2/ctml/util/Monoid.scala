package hkmc2.ctml.util

/** The monoid trait. */
trait Monoid[T](using s: Semigroup[T]):
  export s.*
  /** The identity element. */
  def empty: T

/** Implementation of the `Monoid` trait for `Unit`. */
given Monoid[Unit] with
  def empty = ()

/** Implementation of the `Monoid` trait for `Set`. */
given [T]: Monoid[Set[T]] with
  def empty = Set.empty

/** Implementation of the `Monoid` trait for `List`. */
given [T]: Monoid[List[T]] with
  def empty = List.empty

/** Implementation of the `Monoid` trait for the logical conjunction. */
def AnyMonoid: Monoid[Boolean] = new Monoid(using AnySemigroup):
  def empty = false

/** Implementation of the `Monoid` trait for the logical disjunction. */
def AllMonoid: Monoid[Boolean] = new Monoid(using AllSemigroup):
  def empty = true
