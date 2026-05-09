package hkmc2.ctml.core.var_

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.types.*

/** Global counter used to create unique fresh type variables. */
var freshVarCounter = 0

/** Special fresh type variable renamings that should be applied for debugging. */
val renamings = Map[Int, String]()

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

/** Get a new fresh type variable declaration. */
def declFreshVar(kind: TypeVarKind, original: Option[TypeVar] = None): TypeVarDecl =
  val var_ = newFreshVar()
  declVar(var_, kind, original)

/** Get a new type variable declaration. */
def declVar(var_ : TypeVar, kind: TypeVarKind, original: Option[TypeVar] = None): TypeVarDecl =
  debugTypeVar(TypeVarDecl(var_, kind, original))
