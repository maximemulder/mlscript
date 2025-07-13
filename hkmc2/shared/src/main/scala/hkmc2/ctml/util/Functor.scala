package hkmc2.ctml.util

/** The functor trait */
trait Functor[F[_]]:
  def map[A, B](functor: F[A], f: A => B): F[B]

extension [F[_], A](functor: F[A])(using f: Functor[F])
  def map[B](g: A => B): F[B] =
    f.map(functor, g)

given Functor[List] with
  def map[A, B](list: List[A], f: A => B): List[B] =
    list.map(f)
