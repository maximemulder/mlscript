package hkmc2.ctml.core

/** Get a pretty fresh variable name from a fresh variable index. */
def getFreshVarName(i: Int): String = {
  val greekLetters = List(
    "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
    "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
  )

  if i < greekLetters.size then {
    greekLetters(i)
  } else {
    i.toString()
  }
}

extension [T](iterator: Iterator[T])
  def takeWhileInclusive(p: T => Boolean): Iterator[T] =
    var shouldContinue = true
    iterator.takeWhile((elem) =>
      val result = shouldContinue
      if !p(elem) then shouldContinue = false
      result
    )
