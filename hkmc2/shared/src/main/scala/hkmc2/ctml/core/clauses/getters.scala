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

  /** Get the bounds of a type variable in a given typing direction. */
  def getVarBounds(name: String, dir: Direction): List[Type] =
    clauses
      .varBounds(name)
      .filter(_.dir == dir)
      .map(_.type_)
      .toList

  /** Extract the lower bounds of a type variable from the clauses. */
  def extractVarLowerBounds(name: String): (List[Type], Clauses) =
    clauses.extractVarBounds(name, Direction.Super)

  /** Extract the upper bounds of a type variable from the clauses. */
  def extractVarUpperBounds(name: String): (List[Type], Clauses) =
    clauses.extractVarBounds(name, Direction.Sub)

  def removeTypeVar(varName: String): Clauses =
    // TODO: Shadowing.
    Clauses(clauses.elems.filter(_ match
      case var_ : TypeVar if var_.name == varName =>
        false
      case bound: Bound if bound.name == varName =>
        false
      case _ =>
        true
    ))
