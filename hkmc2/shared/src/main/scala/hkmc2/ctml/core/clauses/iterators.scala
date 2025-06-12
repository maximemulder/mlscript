package hkmc2.ctml.core.clauses

import hkmc2.ctml.types.*

// Iteration methods for clauses.

extension (clauses: Clauses)
  /** Iterate over the term variables defined in the clauses. */
  def termVars: List[TermVar] =
    clauses.elems.iterator.termVars.toList

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: List[TypeVar] =
    clauses.elems.iterator.typeVars.toList

  /** Iterate over the bounds defined in the clauses. */
  def bounds: List[Bound] =
    clauses.elems.iterator.bounds.toList

  /** Iterate over the bounds of a type variable defined in the clauses. */
  def varBounds(varName: String): List[Bound] =
    clauses.elems.iterator.typeVarClauses(varName).typeVarBounds(varName).toList

extension (clauses: Iterator[Clause])
  /** Iterate over the term variables defined in the clauses. */
  def termVars: Iterator[TermVar] =
    clauses.flatMap(_ match
      case var_ : TermVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: Iterator[TypeVar] =
    clauses.flatMap(_ match
      case var_ : TypeVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the bounds defined in the clauses. */
  def bounds: Iterator[Bound] =
    clauses.flatMap(_ match
      case bound : Bound =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the bounds of a variable in the clauses. */
  def typeVarBounds(varName: String): Iterator[Bound] =
    clauses.flatMap(_ match
      case bound : Bound if bound.name == varName =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the sub-clauses in the scope of a type variable in the clauses. */
  def typeVarClauses(varName: String): Iterator[Clause] =
    clauses.takeWhile(_ match
      case var_ : TypeVar if var_.name == varName =>
        false
      case _ =>
        true
    )
