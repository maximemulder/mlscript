package hkmc2.ctml.util

extension (string: String)
  /** Indent a string by the given amount of levels. */
  def indent(n: Int): String =
    val indentation = "  ".repeat(n)
    string.linesIterator.map(line => indentation + line).mkString("\n")
