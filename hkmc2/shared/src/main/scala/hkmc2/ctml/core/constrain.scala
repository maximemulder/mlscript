package hkmc2.ctml.core

import hkmc2.ctml.types.*


/** Constrain a type to be a subtype of another type in a context. */
def constrainSub(sub: Type, sup: Type)(using ctx: Context, mode: Mode): Result[Unit] =

  /* Check the top and bottom types. */

  if sub.is[TBot] then
    return Ok((), Nil)

  if sub.is[TTop] then
    return Ok((), Nil)

  /* Check equal variables*/

  if sub.is[TVar] && sup.is[TVar] && sub.as[TVar].name == sup.as[TVar].name then
    return Ok((), Nil)

  /* Check fresh variables in constraining mode. */

  if sub.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) then
    // TODO: Check and propagate bounds.
    return Ok((), List(Bound(sub.as[TVar].name, Direction.Super, sup)))

  if sup.is[TVar] && ctx.isTypeVarFresh(sup.as[TVar].name) then
    // TODO: Check and propagate bounds.
    return Ok((), List(Bound(sub.as[TVar].name, Direction.Sub, sub)))

  /* Check union and intersection types. */

  var subUnionSplit = splitUnion(sub)
  if subUnionSplit.isDefined then
    None  // TODO

  var supUnionSplit = splitUnion(sup)
  if supUnionSplit.isDefined then
    None  // TODO

  var supInterSplit = splitInter(sup)
  if supInterSplit.isDefined then
    None  // TODO

  var subInterSplit = splitInter(sub)
  if subInterSplit.isDefined then
    None  // TODO

  /* Check constraining types. */

  // TODO

  /* Check rigid variables, or fresh variables in checking mode. */

  if sub.is[TVar] && sup.is[TVar] then
    return constrainSubRigidVar(sub.as[TVar], sup.as[TVar])

  /* Check other types. */

  if sub.is[TFun] && sup.is[TFun] then
    return constrainSubFun(sub.as[TFun], sup.as[TFun])

  Fail

def constrainSubRigidVar(sub: TVar, sup: TVar)(using ctx: Context): Result[Unit] =
  if sub.name == sup.name then
    return Ok((), Nil)

  Fail

def constrainSubFun(sub: TFun, sup: TFun)(using ctx: Context, mode: Mode): Result[Unit] =
  var paramResult = constrainSub(sup.param, sub.param)
  if paramResult.isFail then
    return paramResult

  var bodyResult = constrainSub(sub.body, sup.param)
  if bodyResult.isFail then
    return bodyResult

  Ok((), Nil)

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSub(sub: Type, sup: Type)(using ctx: Context): Boolean =
  given Mode = Mode.Check
  constrainSub(sub, sup).isOk

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEq(left: Type, right: Type)(using ctx: Context): Boolean =
  checkSub(left, right) && checkSub(right, left)
