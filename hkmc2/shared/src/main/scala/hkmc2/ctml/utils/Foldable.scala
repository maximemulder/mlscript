package hkmc2.ctml.utils

/** The foldable trait. */
trait Foldable[F[_]]:
  /***/
  def fold[A, B](foldable: F[A], accumulator: B, f: (A, B) => B): B

extension [F[_], A](foldable: F[A])(using f: Foldable[F])
  def fold[B](accumulator: B, g: (A, B) => B): B =
    f.fold(foldable, accumulator, g)

  /** Combine all the elements of the foldable into a monoid. */
  def foldM()(using m: Monoid[A]): A =
    foldable.fold(m.empty, m.combine)

/** Implementation of the `Foldable` trait for `Set`. */
given Foldable[Set] with
  def fold[A, B](set: Set[A], accumulator: B, f: (A, B) => B): B =
    set.foldRight(accumulator)(f)

/** Implementation of the `Foldable` trait for `List`. */
given Foldable[List] with
  def fold[A, B](list: List[A], accumulator: B, f: (A, B) => B): B =
    list.foldRight(accumulator)(f)
