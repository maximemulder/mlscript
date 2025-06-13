package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Append some clauses at the end of the clauses. */
  def append(newClauses: Clause*): Clauses =
    newClauses.foldLeft(clauses)((clauses, newClause) => clauses.appendOne(newClause))

  /** Concatenate some clauses at the end of the clauses. */
  def concat(newClauses: AsClauses*): Clauses =
    newClauses
      .reverse
      .flatMap(_.asElems)
      .foldRight(clauses)((newClause, clauses) => clauses.appendOne(newClause))

  /** Append a clause at the end of the clauses. */
  def appendOne(newClause: Clause): Clauses =
    newClause match
      case Bound(name, dir, type_) =>
        val boundTypes = clauses.getVarBounds(name, dir)
        // TODO: Propagate constraining bounds.
        val boundType = (type_ :: boundTypes).combineMany(dir)(using clauses)
        val bound = Bound(name, dir, type_)
        Clauses(bound :: clauses.elems)
      case clause =>
        Clauses(clause :: clauses.elems)
