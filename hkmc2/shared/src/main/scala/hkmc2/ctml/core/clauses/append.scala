package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Append some clauses at the end of the clauses. */
  def append(newClauses: Clause*): Clauses =
    newClauses.foldLeft(clauses)((clauses, newClause) => clauses.appendOne(newClause))

  /** Concatenate some clauses at the end of the clauses. */
  def concat(newClauses: (Clauses | List[Clause])*): Clauses =
    newClauses
      .reverse
      .flatMap(_.asElems)
      .foldRight(clauses)((newClause, clauses) => clauses.appendOne(newClause))

  /** Append a clause at the end of the clauses. */
  def appendOne(newClause: Clause): Clauses =
    newClause match
      case bound: Bound =>
        // Do not add a bound to the context if it is already satisfied.
        // TODO: Remove existing bounds if subsumed ?
        //   1. Get existing bounds.
        //   2. If this bound subsumed, do not add it.
        //   3. Filter subsumed bounds.
        //if !clauses.checkBoundSatisfied(bound) then
          Clauses(bound :: clauses.elems)
        //else
        //  clauses
      case clause =>
        Clauses(clause :: clauses.elems)
