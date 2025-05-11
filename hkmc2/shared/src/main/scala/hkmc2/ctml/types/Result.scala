package hkmc2.ctml.types

/** A type constraining or type inference result. */
sealed trait Result[+T]:
  def isOk: Boolean = false
  def isFail: Boolean = false
  def ok: (T, List[Bound]) = throw new Exception("Incorrect result unwrapping")

class Ok[+T](val value: T, val bounds: List[Bound]) extends Result[T]:
  override def isOk = true
  override def ok = (value, bounds)

object Fail extends Result[Nothing]:
  override def isFail = true
