package hkmc2.ctml.util

/** The monoid trait. */
trait Monoid[T]:
  /** The identity element. */
  def empty: T

  /** The associative operation. */
  def combine(a: T, b: T): T

/** The set implementation of the monoid trait. */
implicit def SetMonoid[T]: Monoid[Set[T]] = new Monoid[Set[T]] {
  def empty = Set.empty
  def combine(a: Set[T], b: Set[T]): Set[T] =
    a ++ b
}

/** The list implementation of the monoid trait. */
implicit def ListMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
  def empty = List.empty
  def combine(a: List[T], b: List[T]): List[T] =
    a ++ b
}
