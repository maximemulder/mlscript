package hkmc2.ctml.util

/** The show trait. */
trait Show[T]:
  /** Get the string representation of an object. */
  def show(value: T): String
