package hkmc2.ctml.core

import hkmc2.ctml.types.*

val greekLetters = List(
  "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
  "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
)

// TODO: Do not use a global mutable counter.
var freshVarCounter = 0

/** Get a pretty new fresh variable name. */
def newFreshVarName(): String =
  val i = freshVarCounter
  freshVarCounter += 1

  if i < greekLetters.size then
    greekLetters(i)
  else
    i.toString()

/** Get a fresh typed variable with a new unique name. */
def newInferFreshVar(): TypeVar =
  val varName = newFreshVarName()
  newFreshVar(varName)

/** Get a fresh type variable with a given name. */
def newFreshVar(varName: String): TypeVar =
  TypeVar(varName, TypeVarKind.Fresh)

/** Get a rigid type variable with a given name. */
def newRigidVar(varName: String): TypeVar =
  TypeVar(varName, TypeVarKind.Rigid)
