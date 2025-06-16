package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Append some clauses at the end of the clauses. */
  def append(newClauses: Clause*): Clauses =
    newClauses.foldLeft(clauses)((clauses, newClause) => clauses.appendOne(newClause))

  /** Concatenate some clauses at the end of the clauses. */
  def concatCtx(newClauses: AsClauses*): Clauses =
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

  // TODO: There are two types of concatenation: Add clauses to a context, and add clauses to other clauses.
  // When adding new clauses to a context, we may want to keep the context simplified.
  // When adding clauses to other clauses, we usually only want to concatenate them ?
  // In both cases it is simpler if the lower levels appear on the left in the code.
  def concatElems(others: Clauses): Clauses =
    Clauses(others.elems ::: clauses.elems)
