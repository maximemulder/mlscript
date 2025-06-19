package hkmc2.ctml.core.clauses

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

// Iteration methods for clauses.

extension (clauses: AsClauses2)
  /** Iterate over the term variables declared in the clauses. */
  def termVarDecls: List[TermVarDecl] =
    clauses.asClauses.iterator.termVars.toList

  /** Iterate over the type variables declared in the clauses. */
  def typeVarDecls: List[TypeVarDecl] =
    clauses.asClauses.iterator.typeVars.toList

  /** Iterate over the bounds defined in the clauses. */
  def bounds: List[Bound] =
    clauses.asClauses.iterator.bounds.toList

  /** Iterate over the bounds of a type variable defined in the clauses. */
  def varBounds(var_ : TypeVar): List[Bound] =
    clauses.asClauses.iterator.typeVarClauses(var_).typeVarBounds(var_).toList

  def removeTypeVar(var_ : TypeVar): Clauses =
    Clauses(clauses.asClauses.filterUntilInclusive(
      _ match
        case TypeVarDecl(declVar, _) if declVar == var_ =>
          true
        case _ =>
          false,
      _ match
        case TypeVarDecl(declVar, _) if declVar == var_ =>
          false
        case Bound(boundVar, _, _) if boundVar == var_ =>
          false
        case _ =>
          true
    ).toList)

extension (clauses: Iterator[Clause])
  /** Iterate over the term variables defined in the clauses. */
  def termVars: Iterator[TermVarDecl] =
    clauses.flatMap(_ match
      case var_ : TermVarDecl =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: Iterator[TypeVarDecl] =
    clauses.flatMap(_ match
      case var_ : TypeVarDecl =>
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
  def typeVarBounds(var_ : TypeVar): Iterator[Bound] =
    clauses.flatMap(_ match
      case bound: Bound if bound.var_ == var_ =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the sub-clauses in the scope of a type variable in the clauses. */
  def typeVarClauses(var_ : TypeVar): Iterator[Clause] =
    clauses.takeWhile(_ match
      case TypeVarDecl(declVar, _) if declVar == var_ =>
        false
      case _ =>
        true
    )

extension (clauses: AsClauses2)
  def asClauses: List[Clause] =
    clauses match
      case clause: Clause =>
        List(clause)
      case clauses: List[Clause] =>
        clauses
      case Clauses(clauses) =>
        clauses
