package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (clauses: Clauses)
  // Iterators

  /** Iterate over the term variables defined in the clauses. */
  def termVars: Iterator[TermVar] =
    clauses.elems.iterator.flatMap(_ match
      case var_ : TermVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: Iterator[TypeVar] =
    clauses.elems.iterator.flatMap(_ match
      case var_ : TypeVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the bounds defined in the clauses. */
  def bounds: Iterator[Bound] =
    clauses.elems.iterator.flatMap(_ match
      case bound : Bound =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the bounds of a type variable defined in the clauses. */
  def varBounds(varName: String): Iterator[Bound] =
    // TODO: Shadowing.
    clauses.bounds.filter(_.name == varName)

  // Getters

  /** Get the type of a term variable defined in the clauses. */
  def getVarType(varName: String): Type =
    clauses.termVars.find(_.name == varName) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${varName}' not found in the clauses."))

  /** Get a kind of a type variable defined in the clauses. */
  def getTypeVarKind(varName: String): TypeVarKind =
    clauses.typeVars.find((var_) => var_.name == varName) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${varName}' not found in the clauses."))

  /** Setters */

  def addClause(newClause: Clause*): Clauses =
    // Reverse the new clauses so that the clauses on the left are inserted before those on the
    // right.
    val newClauses = Clauses(newClause.reverse.toList)
    clauses.addClauses(newClauses)

  def addClauses(newClauses: Clauses*): Clauses =
    // Reverse the new clauses so that the clauses on the left are inserted before those on the
    // right.
    val newElems = newClauses.reverse.flatMap(_.elems).toList
    Clauses(newElems ::: clauses.elems)

  def addElems(newElems: List[Clause]*): Clauses =
    clauses.addClauses(newElems.map(new Clauses(_))*)

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
