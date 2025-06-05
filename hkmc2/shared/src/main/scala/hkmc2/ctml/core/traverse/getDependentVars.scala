package hkmc2.ctml.core.traverse

import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Get the list of type variables that directly depend on another type variable in the clauses. */
  def getDependentVars(varName: String): Set[String] =
    clauses.elems.iterator.flatMap(_.getDependentVars(varName)).toSet

extension (clause: Clause)
  /** Get the list of type variables that directly depend on another type variable in the clause. */
  def getDependentVars(varName: String): Set[String] =
    clause match
      case bound: Bound if bound.name != varName && bound.type_.hasVar(varName) =>
        Set(bound.name)
      case _ =>
        Set.empty
