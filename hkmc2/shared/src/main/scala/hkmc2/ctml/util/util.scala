package hkmc2.ctml.util

import java.io.{PrintWriter, StringWriter}
import scala.util.boundary, boundary.break
import scala.collection.mutable.ListBuffer

/** The type-level identity functor.*/
type Id[T] = T

/** The type-level constant functor. */
type Const[T] = [_] =>> T

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

extension [T](list: List[T])
  /** Concatenate an element to the list if it is not already in this list. */
  def concatUnique(element: T): List[T] =
    if !list.exists(_ == element) then
      element :: list
    else
      list

  /** Concatenate some elements to the list if it is not already in this list. */
  def concatAllUnique(elements: Iterable[T]): List[T] =
    elements.foldRight(list)((element, list) => list.concatUnique(element))

extension [T](list: ListBuffer[T])
  /** Append an element to a list if it is not already in this list. */
  def appendUnique(element: T) =
    if !list.exists(_ == element) then
      list.append(element)
    else
      list

  /** Append some elements to a list if they are not already in this list. */
  def appendAllUnique(elements: Iterable[T]) =
    for element <- elements do
      list.appendUnique(element)

  def popFront: Option[T] =
    if list.isEmpty then
      return None

    Some(list.remove(0))
