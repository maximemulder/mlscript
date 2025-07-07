package hkmc2.ctml.util

/** The show trait. */
trait Show[T]:
  /** Get the string representation of an object. */
  def show(value: T): String

extension [T](value: T)(using s: Show[T])
  /** Get the string representation of the object. */
  def show = s.show(value)
