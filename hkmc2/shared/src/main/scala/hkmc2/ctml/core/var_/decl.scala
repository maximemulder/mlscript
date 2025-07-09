package hkmc2.ctml.core.var_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*

val greekLetters = List(
  "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ",
  "ν", "ξ", "ο", "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"
)

// TODO: Do not use a global mutable counter.
var freshVarCounter = 0

/** Get a new unique fresh type variable name. */
def newFreshVarName(): String =
  val i = freshVarCounter
  freshVarCounter += 1

  if i < greekLetters.size then
    greekLetters(i)
  else
    i.toString()

/** Get a new fresh type variable declaration with a new unique name. */
def declNewFreshVar(): TypeVarDecl =
  val name = newFreshVarName()
  declFreshVar(TypeVar(name))

/** Get a new fresh type variable declaration with a given name. */
def declFreshVar(var_ : TypeVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Fresh))

/** Get a new rigid type variable declaration with a given name. */
def declRigidVar(var_ : TypeVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Rigid))
