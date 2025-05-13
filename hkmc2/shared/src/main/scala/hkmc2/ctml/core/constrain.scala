package hkmc2.ctml.core

import hkmc2.ctml.types.*


/** Constrain a type to be a subtype of another type in a context. */
def constrainSub(sub: Type, sup: Type)(using ctx: Context, mode: Mode): List[Bound] =
  // Check the top and bottom types.

  if sub.is[TBot] then
    return Nil

  if sub.is[TTop] then
    return Nil

  // Check equal variables

  if sub.is[TVar] && sup.is[TVar] && sub.as[TVar].name == sup.as[TVar].name then
    return Nil

  // Check fresh variables in constraining mode.

  if sub.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) then
    // TODO: Check and propagate bounds.
    return List(Bound(sub.as[TVar].name, Direction.Sub, sup))

  if sup.is[TVar] && ctx.isTypeVarFresh(sup.as[TVar].name) then
    // TODO: Check and propagate bounds.
    return List(Bound(sub.as[TVar].name, Direction.Super, sub))

  // Check union and intersection types.

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

  // Check constraining types.

  // TODO

  // Check rigid variables, or fresh variables in checking mode.

  if sub.is[TVar] && sup.is[TVar] then
    return constrainSubRigidVar(sub.as[TVar], sup.as[TVar])

  // Check other types.

  if sub.is[TLam] && sup.is[TLam] then
    return constrainSubLam(sub.as[TLam], sup.as[TLam])

  throw new TypeError("Fail default case.")

def constrainSubRigidVar(sub: TVar, sup: TVar)(using ctx: Context): List[Bound] =
  if sub.name == sup.name then
    return Nil

  throw new TypeError("Fail constrain rigid var.")

def constrainSubLam(sub: TLam, sup: TLam)(using ctx: Context, mode: Mode): List[Bound] =
  val paramBounds = constrainSub(sup.param, sub.param)
  val retBounds   = constrainSub(sub.ret,   sup.param)
  return retBounds ::: paramBounds

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSub(sub: Type, sup: Type)(using ctx: Context): Boolean =
  given Mode = Mode.Check

  try
    constrainSub(sub, sup)
    return true
  catch
    case _: TypeError =>
      return false

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEq(left: Type, right: Type)(using ctx: Context): Boolean =
  checkSub(left, right) && checkSub(right, left)
