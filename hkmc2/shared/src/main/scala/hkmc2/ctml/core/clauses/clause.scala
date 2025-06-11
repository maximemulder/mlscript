package hkmc2.ctml.core.clauses

import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (clauses: Clauses)
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

  def compareVarLevels(left: String, right: String): Either[Unit, Unit] =
    val first = clauses.typeVars.findMap((var_) =>
      if var_.name == left then
        Some(Left(()))
      else if var_.name == right then
        Some(Right(()))
      else
        None
    )

    first match
      case Some(either) =>
        either
      case None =>
        throw new TypeError(Some(s"Type variable '${left}' or '${right}' not found in the clauses."))
