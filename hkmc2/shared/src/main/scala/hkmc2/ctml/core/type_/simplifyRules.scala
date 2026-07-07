package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*

/** Simplify a negation after its body has already been simplified. */
def simplifyNegation(body: Type): Type =
  makeNegationType(body)

/** Simplify a lambda after its parameter and return type have already been simplified. */
def simplifyLambda(param: Type, ret: Type): Type =
  makeLambdaType(param, ret)

/** Simplify a join or meet after both operands have already been simplified. */
def simplifyJoint(mode: JointMode, left: Type, right: Type)(using ctx: Context): Type =
  hkmc2.ctml.core.combine.combine(mode, left, right)

/** Simplify a universal type after its body has already been simplified. */
def simplifyUniv(var_ : TypeVar, body: Type)(using ctx: Context): Type =
  // body.getVarPolarities(var_) match
  //   case Polarities(true, true) =>
  //     TUniv(var_, body)
  //   case _ =>
  //     body.inline(var_)
  TUniv(var_, body)

/** Simplify a constrained type after its body and constraint have already been simplified. */
def simplifyConstrained(body: Type, constraint: Constraint)(using ctx: Context): Type =
  if checkConstraint(constraint) then
    body
  else
    makeConstrainedType(body, List(constraint))

/** Simplify a constraining type after its body and constraint have already been simplified. */
def simplifyConstraining(body: Type, constraint: Constraint)(using ctx: Context): Type =
  if checkConstraint(constraint) then
    body
  else
    makeConstrainingType(body, List(constraint))
