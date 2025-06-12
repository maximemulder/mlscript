package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Get the type of a term variable. */
  def getVarType(name: String): Type =
    clauses.termVars.find(_.name == name) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${name}' not found in the clauses."))

  /** Get a kind of a type variable. */
  def getTypeVarKind(name: String): TypeVarKind =
    clauses.typeVars.find((var_) => var_.name == name) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${name}' not found in the clauses."))

  /** Get the bounds of a type variable in a given typing direction. */
  def getVarBounds(name: String, dir: Direction): List[Type] =
    clauses
      .varBounds(name)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Get the bounds of a type variable in a given typing direction as a single type. */
  def getVarBound(name: String, dir: Direction): Type =
    clauses
      .getVarBounds(name: String, dir: Direction)
      .combineMany(dir)(using clauses)

  /** Get all the lower bounds of a type variable. */
  def getVarLowerBounds(name: String): List[Type] =
    clauses.getVarBounds(name, Direction.Super)

  /** Get all the upper bounds of a type variable. */
  def getVarUpperBounds(name: String): List[Type] =
    clauses.getVarBounds(name, Direction.Sub)

  /** Get the lower bound of a type variable as a single type. */
  def getVarLowerBound(name: String): Type =
    clauses.getVarBound(name, Direction.Super)

  /** Get the upper bound of a type variable as a single type. */
  def getVarUpperBound(name: String): Type =
    clauses.getVarBound(name, Direction.Sub)

  /** Extract the lower bounds of a type variable from the clauses. */
  def extractVarLowerBounds(name: String): (List[Type], Clauses) =
    clauses.extractVarBounds(name, Direction.Super)

  /** Extract the upper bounds of a type variable from the clauses. */
  def extractVarUpperBounds(name: String): (List[Type], Clauses) =
    clauses.extractVarBounds(name, Direction.Sub)
