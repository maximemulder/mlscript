package hkmc2.ctml.core

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
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
def newInferFreshVar(): TypeVarDecl =
  val name = newFreshVarName()
  newFreshVar(TVar(name))

/** Get a new fresh type variable declaration. */
def newFreshVar(var_ : TVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Fresh))

/** Get a new rigid type variable declaration. */
def newRigidVar(var_ : TVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Rigid))

/** Join two lists of variables by removing duplicates between those lists. */
def joinVars(lefts: List[TVar], rights: List[TVar]): List[TVar] =
  val filteredRights = rights.filter((right) => !(lefts.exists ((left) => left == right)))
  lefts ::: filteredRights

extension (type_ : Type)(using ctx: Context)
  /** Check whether the type is a class reference. */
  def isClass: Boolean =
    type_ match
      case var_ : TVar =>
        ctx.isVarClass(var_)
      case _ =>
        false
