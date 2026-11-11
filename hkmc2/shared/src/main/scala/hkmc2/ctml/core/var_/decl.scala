package hkmc2.ctml.core.var_

import hkmc2.ctml.config.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** Counter used to create unique fresh type variables in the current thread. */
private val freshVarCounter =
  ThreadLocal.withInitial(() => 0)

/** Reset the fresh type variable counter for the current thread. */
def resetFreshVarCounter(): Unit =
  freshVarCounter.set(0)

/** Special fresh type variable renamings that should be applied for debugging. */
val renamings = Map[Int, String]()

/** Get a new unique fresh type variable name. */
def newFreshVarName(): String =
  val i = freshVarCounter.get()
  freshVarCounter.set(i + 1)
  renamings.get(i) match
    case Some(renaming) =>
      renaming
    case _ =>
      i.toString()

/** Get a new fresh type variable. */
def newFreshVar(): TypeVar =
  TypeVar(newFreshVarName())

extension (ctx: SubContext)
  /** Add a new class declaration to the context. */
  def declClass(name : String, parent: Option[ClassVar]): SubContext =
    ctx.extend(ClassDecl(name, parent))

  /** Add a new type variable declaration to the context. */
  def declTypeVar(var_ : TypeVar, kind: TypeVarKind): SubContext =
    val decl = TypeVarDecl(var_, kind, None, ctx.level)
    debugTypeVar(decl)
    ctx.extend(decl)

  /** Add a new inference type variable declaration to the context. */
  def declInferVar(): TypeVarDecl =
    val var_ = newFreshVar()
    ctx.trail.search.foreach(session => session.inferCount += 1)
    val decl = TypeVarDecl(var_, TypeVarKind.Flex, None, ctx.level)
    debugInferVar(decl)
    decl

  /** Add a new fresh type variable declaration to the context. */
  def declFreshVar(kind: TypeVarKind, original: TypeVar): TypeVarDecl =
    val provenance = original.provenance.copy(role = VarRole.Open(kind),
      context = original.provenance.context ++ ctx.trail.flow + original.provenance.root)
    val fresh = new TypeVar(newFreshVarName(), provenance)
    ctx.trail.search.foreach(_.fresh(provenance))
    val decl = TypeVarDecl(fresh, kind, Some(original), ctx.level)
    debugFreshVar(decl)
    decl

  /** Add a new freshened type variable declaration to the context. */
  def declFreshVars(originals: List[TypeVar], kind: TypeVarKind): List[TypeVarDecl] =
    originals.map(original => ctx.declFreshVar(kind, original))

  /** Add a new extruded type variable declaration to the context. */
  def declExtrudeVar(original: TypeVar, level: Int, pol: Polarity): TypeVarDecl =
    val provenance = original.provenance.copy(role = VarRole.Extrude(pol))
    val var_ = new TypeVar(newFreshVarName(), provenance)
    ctx.trail.search.foreach(_.fresh(provenance))
    val decl = TypeVarDecl(var_, TypeVarKind.Flex, Some(original), level)
    debugExtrudeVar(decl)
    decl
