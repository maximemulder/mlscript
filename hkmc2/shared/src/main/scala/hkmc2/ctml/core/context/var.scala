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
  def getTypeVarKind(var_ : TVar): TypeVarKind =
    ctx.clauses.typeVarDecls.find(_.var_ == var_) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${var_}' not found in the context."))

  /** Get the bounds of a type variable in a given typing direction. */
  def getVarBounds(var_ : TVar, dir: Direction): List[Type] =
    ctx
      .clauses
      .varBounds(var_)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Get the bounds of a type variable in a given typing direction as a single type. */
  def getVarBound(var_ : TVar, dir: Direction): Type =
    ctx
      .getVarBounds(var_ : TVar, dir: Direction)
      .combineMany(dir)(using ctx)

  /** Get all the lower bounds of a type variable. */
  def getVarLowerBounds(var_ : TVar): List[Type] =
    ctx.getVarBounds(var_, Direction.Super)

  /** Get all the upper bounds of a type variable. */
  def getVarUpperBounds(var_ : TVar): List[Type] =
    ctx.getVarBounds(var_, Direction.Sub)

  /** Get the lower bound of a type variable as a single type. */
  def getVarLowerBound(var_ : TVar): Type =
    ctx.getVarBound(var_, Direction.Super)

  /** Get the upper bound of a type variable as a single type. */
  def getVarUpperBound(var_ : TVar): Type =
    ctx.getVarBound(var_, Direction.Sub)
