package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

// Getter methods for clauses.

extension (clauses: Clauses)
  /** Get the type of a term variable. */
  def getVarType(varName: String): Type =
    clauses.termVars.find(_.name == varName) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${varName}' not found in the clauses."))

  /** Get a kind of a type variable. */
  def getTypeVarKind(varName: String): TypeVarKind =
    clauses.typeVars.find((var_) => var_.name == varName) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${varName}' not found in the clauses."))

  /** Get the bounds of a type variable in a given typing direction. */
  def getVarBounds(varName: String, dir: Direction): List[Type] =
    clauses
      .varBounds(varName)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Get the bounds of a type variable in a given typing direction as a single type. */
  def getVarBound(varName: String, dir: Direction): Type =
    clauses
      .getVarBounds(varName: String, dir: Direction)
      .mergeMany(dir)(using clauses)

  /** Get all the lower bounds of a type variable. */
  def getVarLowerBounds(varName: String): List[Type] =
    clauses.getVarBounds(varName, Direction.Super)

  /** Get all the upper bounds of a type variable. */
  def getVarUpperBounds(varName: String): List[Type] =
    clauses.getVarBounds(varName, Direction.Sub)

  /** Get the lower bound of a type variable as a single type. */
  def getVarLowerBound(varName: String): Type =
    clauses.getVarBound(varName, Direction.Super)

  /** Get the upper bound of a type variable as a single type. */
  def getVarUpperBound(varName: String): Type =
    clauses.getVarBound(varName, Direction.Sub)
