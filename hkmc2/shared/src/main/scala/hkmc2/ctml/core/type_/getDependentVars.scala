package hkmc2.ctml.core.type_

import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Get the list of type variables that directly depend on another type variable in the clauses. */
  def getDependentVars(var_ : TVar): Set[TVar] =
    clauses.elems.iterator.flatMap(_.getDependentVars(var_)).toSet

extension (clause: Clause)
  /** Get the list of type variables that directly depend on another type variable in the clause. */
  def getDependentVars(var_ : TVar): Set[TVar] =
    clause match
      case Bound(boundVar, _ ,boundType) if boundVar != var_ && boundType.hasVar(var_) =>
        Set(boundVar)
      case _ =>
        Set.empty
