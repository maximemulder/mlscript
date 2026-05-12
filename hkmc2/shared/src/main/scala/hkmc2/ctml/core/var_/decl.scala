package hkmc2.ctml.core.var_

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
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

extension (ctx: Context)
  /** Add a new class declaration to the context. */
  def declClass(var_ : TypeVar, parent: Option[TypeVar]): Context =
    ctx.extend(debugTypeVar(TypeVarDecl(var_, TypeVarKind.Class(parent), None, None)))

  /** Add a new fresh type variable declaration to the context. */
  def declFreshVar(kind: TypeVarKind, original: Option[TypeVar] = None): (TypeVar, Context) =
    val var_ = newFreshVar()
    (var_, ctx.declVar(var_, kind, original))

  /** Add a new type variable declaration to the context. */
  def declVar(var_ : TypeVar, kind: TypeVarKind, original: Option[TypeVar] = None): Context =
    val level = ctx.getMaxLevel() + 1
    ctx.extend(debugTypeVar(TypeVarDecl(var_, kind, original, Some(level))))
