package hkmc2.ctml.core.var_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*

// TODO: Do not use a global mutable counter.
var freshVarCounter = 0

// Special variable renamings that should be applied for debugging.
val renamings = Map[Int, String](
  // 32 -> "A",
  // 46 -> "B",
  // 53 -> "C",
)

/** Get a new unique fresh type variable name. */
def newFreshVarName(): String =
  val i = freshVarCounter
  freshVarCounter += 1
  renamings.get(i) match
    case Some(renaming) =>
      renaming
    case _ =>
      i.toString()

/** Get a new fresh type variable. */
def newFreshVar(): TypeVar =
  TypeVar(newFreshVarName())

/** Get a new fresh rigid type variable declaration. */
def declFreshRigidVar(original: Option[TypeVar] = None): TypeVarDecl =
  val var_ = newFreshVar()
  declRigidVar(var_, original)

/** Get a new fresh flexible type variable declaration. */
def declFreshFlexVar(original: Option[TypeVar] = None): TypeVarDecl =
  val var_ = newFreshVar()
  declFlexVar(var_, original)

/** Get a new rigid type variable declaration. */
def declRigidVar(var_ : TypeVar, original: Option[TypeVar] = None): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Rigid, original))

/** Get a new flexible type variable declaration. */
def declFlexVar(var_ : TypeVar, original: Option[TypeVar] = None): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Flex, original))
