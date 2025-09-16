package hkmc2.ctml.core.type_

import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  /** Get the list of type variables that directly depend on another type variable in the clauses. */
  def getDependentVars(var_ : TypeVar): Set[TypeVar] =
    clauses.elems.iterator.flatMap(_.getDependentVars(var_)).toSet

extension (clause: Clause)
  /** Get the list of type variables that directly depend on another type variable in the clause. */
  def getDependentVars(var_ : TypeVar): Set[TypeVar] =
    clause match
      case Bound(boundVar, _ ,boundType) if boundVar != var_ && boundType.containsVar(var_) =>
        Set(boundVar)
      case _ =>
        Set.empty
