package hkmc2.ctml.util

import java.io.{PrintWriter, StringWriter}

/** . */
def getStackTraceString(throwable: Throwable): String =
  val stringWriter = new StringWriter()
  val printWriter = new PrintWriter(stringWriter)
  throwable.printStackTrace(printWriter)
  stringWriter.toString

extension [T](iterator: Iterator[T])
  /** Iterate over the elements of an iterator until a predicate fails, including the element for
   *  which the predicate failed. */
  def takeWhileInclusive(p: T => Boolean): Iterator[T] =
    var shouldContinue = true
    iterator.takeWhile((elem) =>
      if !shouldContinue then
        false
      else
        if !p(elem) then
          shouldContinue = false
        true
    )

  /** Apply a function to the elements of an iterator and return the first non-none result. */
  def findMap[U](f: T => Option[U]): Option[U] =
    iterator.flatMap(f).nextOption()

extension [T](list: List[T])
  def fold1Right(f: (T, T) => T): T =
    list match
      case Nil =>
        throw new Exception("Called fold1Right on an empty list.")
      case head :: Nil  =>
        head
      case head :: tail =>
        f(head, tail.fold1Right(f))
