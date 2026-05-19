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
  def declClass(name : String, parent: Option[ClassVar]): Context =
    ctx.extend(ClassDecl(name, parent))

  /** Add a new fresh type variable declaration to the context. */
  def declFreshVar(kind: TypeVarKind, original: Option[TypeVar] = None, level: Option[Int] = None): (TypeVar, Context) =
    val var_ = newFreshVar()
    (var_, ctx.declVar(var_, kind, original, level))

  /** Add a new type variable declaration to the context. */
  def declVar(var_ : TypeVar, kind: TypeVarKind, original: Option[TypeVar] = None, level: Option[Int] = None): Context =
    val varLevel = level.getOrElse(ctx.getMaxLevel() + 1)
    ctx.extend(debugTypeVar(TypeVarDecl(var_, kind, original, varLevel)))
