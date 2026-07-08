package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.impls.simplify.simplify

/** Simplify a negation after its body has already been simplified. */
def simplifyNegation(body: Type): Type =
  makeNegationType(body)

/** Simplify a lambda after its parameter and return type have already been simplified. */
def simplifyLambda(param: Type, ret: Type): Type =
  makeLambdaType(param, ret)

/** Simplify a join or meet after both operands have already been simplified. */
def simplifyJoint(mode: JointMode, left: Type, right: Type)(using ctx: Context): Type =
  hkmc2.ctml.core.combine.combine(mode, left, right)

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
