package hkmc2.ctml.utils

/** The tree trait. */
trait Tree[T]:
  /** The children of a node. */
  def children(value: T): List[T]

/** Implementation of the `Show` trait for `Tree`. */
def TreeShow[T](using t: Tree[T], s: Show[T]): Show[T] = new Show:
  override def show(tree: T): String =
    val builder = StringBuilder()
    showTree(tree, 0, builder)
    builder.toString()

  /** Convert a tree to a string at a given level. */
  private def showTree(tree: T, level: Int, builder: StringBuilder): Unit =
    builder.append(s.show(tree).addIndentation(level))
    builder.append("\n")
    for child <- t.children(tree) do
      showTree(child, level + 1, builder)
