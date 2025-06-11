package hkmc2.ctml.core.clauses

import hkmc2.ctml.types.*

// Getter methods for clauses.

extension (clauses: Clauses)
  /** Get the type of a term variable defined in the clauses. */
  def getVarType(varName: String): Type =
    clauses.termVars.find(_.name == varName) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(Some(s"Variable '${varName}' not found in the clauses."))

  /** Get a kind of a type variable defined in the clauses. */
  def getTypeVarKind(varName: String): TypeVarKind =
    clauses.typeVars.find((var_) => var_.name == varName) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(Some(s"Type variable '${varName}' not found in the clauses."))
