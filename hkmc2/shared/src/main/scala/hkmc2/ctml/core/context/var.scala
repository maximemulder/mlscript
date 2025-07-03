package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (ctx: Context)
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
