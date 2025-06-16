package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Get the type of a term variable. */
  def getVarType(name: String): Type =
    ctx.clauses.termVars.find(_.name == name) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${name}' not found in the context."))

  /** Get a kind of a type variable. */
  def getTypeVarKind(name: String): TypeVarKind =
    ctx.clauses.typeVars.find((var_) => var_.name == name) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${name}' not found in the context."))

  /** Get the bounds of a type variable in a given typing direction. */
  def getVarBounds(name: String, dir: Direction): List[Type] =
    ctx
      .clauses
      .varBounds(name)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Get the bounds of a type variable in a given typing direction as a single type. */
  def getVarBound(name: String, dir: Direction): Type =
    ctx
      .getVarBounds(name: String, dir: Direction)
      .combineMany(dir)(using ctx)

  /** Get all the lower bounds of a type variable. */
  def getVarLowerBounds(name: String): List[Type] =
    ctx.getVarBounds(name, Direction.Super)

  /** Get all the upper bounds of a type variable. */
  def getVarUpperBounds(name: String): List[Type] =
    ctx.getVarBounds(name, Direction.Sub)

  /** Get the lower bound of a type variable as a single type. */
  def getVarLowerBound(name: String): Type =
    ctx.getVarBound(name, Direction.Super)

  /** Get the upper bound of a type variable as a single type. */
  def getVarUpperBound(name: String): Type =
    ctx.getVarBound(name, Direction.Sub)
