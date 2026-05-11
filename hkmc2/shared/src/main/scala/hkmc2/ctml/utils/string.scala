package hkmc2.ctml.utils

extension (string: String)
  /** Indent a string by the given amount of levels. */
  def addIndentation(n: Int): String =
    val indentation = "  ".repeat(n)
    string.linesIterator.map(line => indentation + line).mkString("\n")

  /** Replace all digits in a string by their subscript version. */
  def toSubscript(): String =
    var newString = string
    for (original, replacement) <- List(
      ("0", "₀"),
      ("1", "₁"),
      ("2", "₂"),
      ("3", "₃"),
      ("4", "₄"),
      ("5", "₅"),
      ("6", "₆"),
      ("7", "₇"),
      ("8", "₈"),
      ("9", "₉"),
    ) do
      newString = newString.replaceAll(original, replacement)

    newString

  /** Replace all digits in a string by their superscript version. */
  def toSuperscript(): String =
    var newString = string
    for (original, replacement) <- List(
      ("0", "⁰"),
      ("1", "¹"),
      ("2", "²"),
      ("3", "³"),
      ("4", "⁴"),
      ("5", "⁵"),
      ("6", "⁶"),
      ("7", "⁷"),
      ("8", "⁸"),
      ("9", "⁹"),
    ) do
      newString = newString.replaceAll(original, replacement)

    newString
