package hkmc2.ctml.core.context

import hkmc2.ctml.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Get the type of a term variable. */
  def getVarType(name: String): Type =
    ctx.clauses.termVarDecls.find(_.name == name) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new Exception(s"Variable '${name}' not found in the context.")

  /** Get the class definition of a class variable. */
  def getClassDef(var_ : ClassVar): ClassDecl =
    ctx.clauses.classDefs.find(_.name == var_.name) match
      case Some(decl) =>
        decl
      case None =>
        throw new Exception(s"Class '${var_}' not found in the context.")

  /** Get the declaration of a type variable in the context. */
  def getTypeVarDecl(var_ : TypeVar): TypeVarDecl =
    ctx.clauses.typeVarDecls.find(_.var_ == var_) match
      case Some(decl) =>
        decl
      case None =>
        throw new Exception(s"Type variable '${var_}' not found in the context.")

  /** Get the kind of a type variable in the context. */
  def getTypeVarKind(var_ : TypeVar): TypeVarKind =
    ctx.getTypeVarDecl(var_).kind

  /** Get the level of a type variable in the context. */
  def getTypeVarLevel(var_ : TypeVar): Int =
    ctx.getTypeVarDecl(var_).level

  /** Get the level of a type variable in the context. */
  def getTypeVarOrigin(var_ : TypeVar): Option[TypeVar] =
    ctx.clauses.typeVarDecls.find(_.var_ == var_).flatMap(_.origin)

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
  /** Get the kind of the type variable in the context. */
  def kind: TypeVarKind =
    ctx.getTypeVarKind(var_)

  /** Get the level of the type variable in the context. */
  def level: Int =
    ctx.getTypeVarLevel(var_)

  /** Get the origin of the type variable in the context. */
  def origin: Option[TypeVar] =
    ctx.getTypeVarOrigin(var_)

  /** Check whether the type variable is a flexible type variable. */
  def isFlex: Boolean =
    var_.kind == TypeVarKind.Flex

  /** Check whether the type variable is a rigid type variable. */
  def isRigid: Boolean =
    var_.kind == TypeVarKind.Rigid

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

extension (type_ : Type)(using ctx: Context)
  /** Get the lowest polymorphic level of the type variables referenced in this type, if any. */
  def lowLevel: Option[Int] =
    type_.getVars
      .map(_.level)
      .minOption

  /** Get the highest polymorphic level of the type variables referenced in this type, if any. */
  def highLevel: Option[Int] =
    type_.getVars
      .map(_.level)
      .maxOption

extension (bound: Bound)(using ctx: Context)
  /** Get the lowest polymorphic level of the type variables referenced in this bound. */
  def lowLevel: Int =
    Iterator.single(bound.var_.level)
      .concat(bound.type_.lowLevel)
      .min

  /** Get the highest polymorphic level of the type variables referenced in this bound. */
  def highLevel: Int =
    Iterator.single(bound.var_.level)
      .concat(bound.type_.highLevel)
      .max
