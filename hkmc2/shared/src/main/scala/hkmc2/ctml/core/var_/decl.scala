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

  /** Add a new type variable declaration to the context. */
  def declTypeVar(var_ : TypeVar, kind: TypeVarKind): Context =
    val level = ctx.maxLevel + 1
    val decl = TypeVarDecl(var_, kind, None, level)
    debugTypeVar(decl)
    ctx.extend(decl)

  /** Add a new inference type variable declaration to the context. */
  def declInferVar(): TypeVarDecl =
    val level = ctx.maxLevel + 1
    val var_ = newFreshVar()
    val decl = TypeVarDecl(var_, TypeVarKind.Flex, None, level)
    debugInferVar(decl)
    decl

  /** Add a new fresh type variable declaration to the context. */
  def declFreshVar(level: Int, kind: TypeVarKind, original: TypeVar): TypeVarDecl =
    val decl = TypeVarDecl(newFreshVar(), kind, Some(original), level)
    debugFreshVar(decl)
    decl

  /** Add a new freshened type variable declaration to the context. */
  def declFreshVars(originals: List[TypeVar], kind: TypeVarKind): List[TypeVarDecl] =
    val level = ctx.maxLevel + 1
    val decls = originals.map((original) => TypeVarDecl(newFreshVar(), kind, Some(original), level))
    for decl <- decls do
      debugFreshVar(decl)

    decls

  /** Add a new extruded type variable declaration to the context. */
  def declExtrudeVar(original: TypeVar, level: Int): TypeVarDecl =
    val var_ = newFreshVar()
    val decl = TypeVarDecl(var_, TypeVarKind.Flex, Some(original), level)
    debugExtrudeVar(decl)
    decl
