package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.config.*

extension (ctx: Context)
  /** Make a type variable flexible. */
  def flexify(var_ : TypeVar): Context =
    ctx.mapClauses(_ match
      case TypeVarDecl(ctxvar, TypeVarKind.Rigid, original) if ctxvar == var_ =>
        TypeVarDecl(var_, TypeVarKind.Flex, original)
      case clause =>
        clause
    )

  /** Make some type variables flexible. */
  def flexify(vars: Iterable[TypeVar]): Context =
    vars.foldLeft(ctx)((ctx, var_) => ctx.flexify(var_))

  /** Get the type of a term variable. */
  def getVarType(name: String): Type =
    ctx.clauses.termVarDecls.find(_.name == name) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${name}' not found in the context."))

  /** Get a kind of a type variable. */
  def getTypeVarKind(var_ : TypeVar): TypeVarKind =
    ctx.clauses.typeVarDecls.find(_.var_ == var_) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${var_}' not found in the context."))

  /** Get all the bounds of a type variable in a given typing direction. */
  def getAllVarBounds(var_ : TypeVar, dir: Direction): List[Type] =
    ctx
      .clauses
      .varBounds(var_)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Get all the lower bounds of a type variable. */
  def getAllVarLowerBounds(var_ : TypeVar): List[Type] =
    ctx.getAllVarBounds(var_, Direction.Super)

  /** Get all the upper bounds of a type variable. */
  def getAllVarUpperBounds(var_ : TypeVar): List[Type] =
    ctx.getAllVarBounds(var_, Direction.Sub)

  /** Get the bound of a type variable in a given typing direction. */
  def getVarBound(var_ : TypeVar, dir: Direction): Type =
    ctx.clauses.varBound(var_, dir) match
      case Some(type_) =>
        type_
      case None =>
        getExtremalType(dir)

  /** Get the lower bound of a type variable. */
  def getVarLowerBound(var_ : TypeVar): Type =
    ctx.getVarBound(var_, Direction.Super)

  /** Get the upper bound of a type variable. */
  def getVarUpperBound(var_ : TypeVar): Type =
    ctx.getVarBound(var_, Direction.Sub)

extension (type_ : Type)(using ctx: Context)
  /** Check whether the type is a class type variable. */
  def isClassVar: Boolean =
    type_ match
      case TVar(var_) =>
        var_.isClass
      case _ =>
        false

  /** Check whether the type is a flexible type variable. */
  def isFlexVar: Boolean =
    type_ match
      case TVar(var_) =>
        var_.isFlex
      case _ =>
        false

  /** Check whether the type is a rigid type variable. */
  def isRigidVar: Boolean =
    type_ match
      case TVar(var_) =>
        var_.isRigid
      case _ =>
        false

extension (var_ : TypeVar)(using ctx: Context)
  /** Check whether the type variable is a class type variable. */
  def isClass: Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Class

  /** Check whether the type variable is a flexible type variable. */
  def isFlex: Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Flex

  /** Check whether the type variable is a rigid type variable. */
  def isRigid: Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Rigid

  /** Check whether the type variable is recursive. */
  def isRecursive: Boolean =
    ctx.isVarRecursive(var_)

  /** Get the bound of a type variable in a given direction. */
  def bound(dir: Direction) =
    ctx.getVarBound(var_, dir)

  /** Get the lower bound of the type variable. */
  def lowerBound: Type =
    ctx.getVarLowerBound(var_)

  /** Get the upper bound of the type variable. */
  def upperBound: Type =
    ctx.getVarUpperBound(var_)
