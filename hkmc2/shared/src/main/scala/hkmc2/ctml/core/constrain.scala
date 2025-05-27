package hkmc2.ctml.core

import hkmc2.ctml.types.*


/** Constrain a type to be a subtype of another type in a context. */
def constrainSub(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  constrainSubImpl(sub, sup)

/** Implementation of `constrainSub`. */
def constrainSubImpl(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  // Check the top and bottom types.

  if sub.is[TBot] then
    return Clauses.none

  if sup.is[TTop] then
    return Clauses.none

  // Check equal variables

  if sub.is[TVar] && sup.is[TVar] && sub.as[TVar].name == sup.as[TVar].name then
    return Clauses.none

  // Check fresh variables in constraining mode.

  if mode == Mode.Constrain then
    if sub.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) then
      // TODO: Check and propagate bounds.
      return Clauses(List(Bound(sub.as[TVar].name, Direction.Sub, sup)))

    if sup.is[TVar] && ctx.isTypeVarFresh(sup.as[TVar].name) then
      // TODO: Check and propagate bounds.
      return Clauses(List(Bound(sup.as[TVar].name, Direction.Super, sub)))

  // Check union and intersection types.

  var subUnionSplit = splitUnion(sub)
  if subUnionSplit.isDefined then
    val (subLeft, subRight) = subUnionSplit.get
    return ctx.all(
      () => constrainSub(subLeft,  sup),
      () => constrainSub(subRight, sup),
    )

  var supUnionSplit = splitUnion(sup)
  if supUnionSplit.isDefined then
    val (supLeft, supRight) = supUnionSplit.get
    return ctx.any(
      () => constrainSub(sub, supLeft),
      () => constrainSub(sub, supRight),
    )

  var supInterSplit = splitInter(sup)
  if supInterSplit.isDefined then
    val (supLeft, supRight) = supInterSplit.get
    return ctx.all(
      () => constrainSub(sub, supLeft),
      () => constrainSub(sub, supRight),
    )

  var subInterSplit = splitInter(sub)
  if subInterSplit.isDefined then
    val (subLeft, subRight) = subInterSplit.get
    return ctx.any(
      () => constrainSub(subLeft, sup),
      () => constrainSub(subRight, sup),
    )

  // Check constrained types.

  if sup.is[TConstrained] then
    return constrainSubConstrainedSup(sub, sup.as[TConstrained])

  if sub.is[TConstrained] then
    return constrainSubConstrainedSup(sub.as[TConstrained], sup)

  // Check constraining types.

  // TODO

  // Check rigid variables, or fresh variables in checking mode.

  if sub.is[TVar] && sup.is[TVar] then
    return constrainSubRigidVar(sub.as[TVar], sup.as[TVar])

  // Check other types.

  if sub.is[TLam] && sup.is[TLam] then
    return constrainSubLam(sub.as[TLam], sup.as[TLam])

  throw new TypeError(s"Fail default case ${sub} ≤ ${sup}.")

def constrainSubRigidVar(sub: TVar, sup: TVar)(using ctx: Clauses): Clauses =
  if sub.name == sup.name then
    return Clauses.none

  throw new TypeError("Fail constrain rigid var.")

/** Constrain a constrained type to be a subtype of another type. */
def constrainSubConstrainedSup(sub: TConstrained, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  val varsClauses = sub.vars.map(newFreshVar(_))
  given Clauses = ctx.addElems(varsClauses, sub.bounds)
  constrainSub(sub.base, sup)

/** Constrain a type to be a subtype of a constrained type. */
def constrainSubConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Clauses, mode: Mode): Clauses =
  val varsClauses = sup.vars.map(newRigidVar(_))
  given Clauses = ctx.addElems(varsClauses, sup.bounds)
  constrainSub(sub, sup.base)

/** Constrain a lambda type to be a subtype of another lambda type. */
def constrainSubLam(sub: TLam, sup: TLam)(using ctx: Clauses, mode: Mode): Clauses =
  val paramClauses = constrainSub(sup.param, sub.param)
  val retClauses   = constrainSub(sub.ret,   sup.ret)
  return paramClauses.addClauses(retClauses)

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSub(sub: Type, sup: Type)(using ctx: Clauses): Boolean =
  given Mode = Mode.Check
  try
    constrainSub(sub, sup)
  catch
    case _: TypeError =>
      return false

  return true

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEq(left: Type, right: Type)(using ctx: Clauses): Boolean =
  checkSub(left, right) && checkSub(right, left)

extension (ctx: Clauses)
  /** Check if a bound is satisified in the context. */
  def checkBoundSatisfied(bound: Bound) =
    val var_ = TVar(bound.name)
    bound.dir match
      case Direction.Sub =>
        given Clauses = ctx
        checkSub(var_, bound.type_)
      case Direction.Super =>
        given Clauses = ctx
        checkSub(bound.type_, var_)
