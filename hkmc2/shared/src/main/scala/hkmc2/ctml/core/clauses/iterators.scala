package hkmc2.ctml.core.clauses

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

// Iteration methods for clauses.

extension (clauses: AsClauses)
  /** Iterate over the term variables defined in the clauses. */
  def termVars: List[TermVar] =
    clauses.asClauses.iterator.termVars.toList

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: List[TypeVar] =
    clauses.asClauses.iterator.typeVars.toList

  /** Iterate over the bounds defined in the clauses. */
  def bounds: List[Bound] =
    clauses.asClauses.iterator.bounds.toList

  /** Iterate over the bounds of a type variable defined in the clauses. */
  def varBounds(name: String): List[Bound] =
    clauses.asClauses.iterator.typeVarClauses(name).typeVarBounds(name).toList

  def extractVarBounds(name: String, dir: Direction): (List[Type], Clauses) =
    val (types, newClauses) = clauses.asClauses.extractUntil(
      _ match
        case TypeVar(varName, _) if varName == name =>
          false
        case _ =>
          true,
      _ match
        case Bound(boundName, boundDir, type_) if boundName == name && boundDir == dir =>
          Some(type_)
        case _ =>
          None,
    )

    (types.toList, Clauses(newClauses.toList))

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
      case bound: Bound =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the bounds of a variable in the clauses. */
  def typeVarBounds(name: String): Iterator[Bound] =
    clauses.flatMap(_ match
      case bound: Bound if bound.name == name =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the sub-clauses in the scope of a type variable in the clauses. */
  def typeVarClauses(name: String): Iterator[Clause] =
    clauses.takeWhile(_ match
      case TypeVar(varName, _) if varName == name =>
        false
      case _ =>
        true
    )

extension (clauses: AsClauses)
  def asClauses: List[Clause] =
    clauses match
      case clause: Clause =>
        List(clause)
      case clauses: List[Clause] =>
        clauses
      case Clauses(clauses) =>
        clauses
