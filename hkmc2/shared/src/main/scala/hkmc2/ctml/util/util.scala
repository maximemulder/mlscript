package hkmc2.ctml.util

import java.io.{PrintWriter, StringWriter}
import scala.util.boundary, boundary.break
import scala.collection.mutable.ListBuffer

/** Get the stack trace of an exception as a string. */
def getStackTraceString(throwable: Throwable): String =
  val stringWriter = new StringWriter()
  val printWriter  = new PrintWriter(stringWriter)
  throwable.printStackTrace(printWriter)
  stringWriter.toString()

extension [T](iterable: Iterable[T])
  /** Find and map th first element of an iterable that satisfies a function. */
  def findMap[U](f: T => Option[U]): Option[U] =
    iterable.iterator.flatMap(f).nextOption()

  /** Extract some elements of an iterable using an extraction function. */
  def extract[U](extract: T => Option[U]): (Iterable[U], Iterable[T]) =
    iterable.partitionMap(element => extract(element) match
      case Some(value) =>
        Left(value)
      case None =>
        Right(element)
    )

  /** Extract some elements of an iterable using an extraction function until a condition is met.
   */
  def extractUntil[U](p: T => Boolean, f: T => Option[U]): (Iterable[U], Iterable[T]) =
    var finished = false

    def extractClosure(element: T): Option[U] =
      if finished then
        return None

      if p(element) then
        finished = true
        return None

      f(element)

    iterable.extract(extractClosure)

  /** Filter some elements of an iterable using a filtering function until a condition is met.
   */
  def filterUntilInclusive(p: T => Boolean, f: T => Boolean): Iterable[T] =
    var finished = false

    def filterClosure(element: T): Boolean =
      if finished then
        return true

      if p(element) then
        finished = true

      f(element)

    iterable.filter(filterClosure)

extension [T](list: List[T])
  def fold1Right(f: (T, T) => T): T =
    list match
      case Nil =>
        throw new Exception("Called fold1Right on an empty list.")
      case head :: Nil  =>
        head
      case head :: tail =>
        f(head, tail.fold1Right(f))

/** Concatenate two lists while removing elements in the right list that are already in the left
 *  list.
 */
def concatDistinct[T](lefts: List[T], rights: List[T]): List[T] =
  val filteredRights = rights.filter((right) => !(lefts.exists ((left) => left == right)))
  lefts ::: filteredRights
