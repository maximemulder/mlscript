package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Get a fresh typed variable with a new unique name. */
def newInferFreshVar(): TypeVar =
  val varName = newFreshVarName()
  newFreshVar(varName)

/** Get a fresh type variable with a given name. */
def newFreshVar(varName: String): TypeVar =
  TypeVar(varName, TypeVarKind.Fresh)

/** Get a rigid type variable with a given name. */
def newRigidVar(varName: String): TypeVar =
  TypeVar(varName, TypeVarKind.Rigid)
