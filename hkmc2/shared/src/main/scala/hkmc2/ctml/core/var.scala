package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
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
  debugTypeVar(TypeVar(varName, TypeVarKind.Fresh))

/** Get a rigid type variable with a given name. */
def newRigidVar(varName: String): TypeVar =
  debugTypeVar(TypeVar(varName, TypeVarKind.Rigid))

/** Join two lists of variables by removing duplicates between those lists. */
def joinVars(lefts: List[String], rights: List[String]): List[String] =
  val filteredRights = rights.filter((right) => !(lefts.exists ((left) => left == right)))
  lefts ::: filteredRights

extension (type_ : Type)(using ctx: Clauses)
  /** Check whether the type is a class reference. */
  def isClass: Boolean =
    type_ match
      case TVar(name) =>
        ctx.isVarClass(name)
      case _ =>
        false
