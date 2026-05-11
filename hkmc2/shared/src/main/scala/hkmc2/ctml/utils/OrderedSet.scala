package hkmc2.ctml.utils

import scala.collection.mutable.ListBuffer

/** A mutable set class that preserves insertion order. */
class OrderedSet[T](elements: ListBuffer[T] = ListBuffer[T]()):
  /** Check whether the set contains a given value. */
  def contains(value: T): Boolean =
    this.elements.exists(_ == value)

  /** Add a value to the set. Return `true` if the value was added, or `false` if it was already
   *  present. */
  def add(value: T): Boolean =
    if this.contains(value) then
      return false

    this.elements.append(value)
    return true

  /** Add some values to the set. Return the set itself. */
  def addAll(values: IterableOnce[T]): OrderedSet[T] =
    for value <- values.iterator do
      this.add(value)

    return this
