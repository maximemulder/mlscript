package hkmc2.ctml.util

/** The tree trait. */
trait Tree[T]:
  /** The children of a node. */
  def children(value: T): List[T]
