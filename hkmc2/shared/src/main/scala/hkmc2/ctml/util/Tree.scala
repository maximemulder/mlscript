package hkmc2.ctml.util

/** The tree trait. */
trait Tree[T]:
  /** The children of a node. */
  def children(value: T): List[T]

/** Implementation of the `Show` trait for `Tree`. */
def TreeShow[T](using t: Tree[T], s: Show[T]): Show[T] = new Show:
  override def show(tree: T): String =
    val builder = StringBuilder(s.show(tree))
    val children = t.children(tree)
    if children != List.empty then
      builder.append(" ")
      builder.append(children)

    builder.toString()
