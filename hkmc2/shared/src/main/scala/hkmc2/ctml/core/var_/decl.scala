package hkmc2.ctml.core.var_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*

// TODO: Do not use a global mutable counter.
var freshVarCounter = 0

/** Get a new unique fresh type variable name. */
def newFreshVarName(): String =
  val i = freshVarCounter
  freshVarCounter += 1
  i.toString()

/** Get a new fresh flexible type variable declaration with a new unique name. */
def declNewFreshVar(): TypeVarDecl =
  val name = newFreshVarName()
  declFlexVar(TypeVar(name))

/** Get a new flexible type variable declaration with a given name. */
def declFlexVar(var_ : TypeVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Flex))

/** Get a new rigid type variable declaration with a given name. */
def declRigidVar(var_ : TypeVar): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, TypeVarKind.Rigid))
