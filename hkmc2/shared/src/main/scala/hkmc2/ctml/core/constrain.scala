package hkmc2.ctml.core

import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def constrainDir(sub: Type, sup: Type, dir: Direction)(using ctx: Clauses, mode: Mode): Clauses =
  dir match
    case Direction.Sub =>
      constrainSub(sub, sup)
    case Direction.Super =>
      constrainSub(sup, sub)

/** Constrain a type to be a subtype of another type in a context. */
def constrainSub(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  try
    constrainSubImpl(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(sub, sup)
      throw error

/** Implementation of `constrainSub`. */
def constrainSubImpl(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  // Subtyping of constraining types.

  if sub.is[TConstraining] || sup.is[TConstraining] then
    val (subBase, subBounds) = splitConstrainings(sub)
    val (supBase, supBounds) = splitConstrainings(sup)
    val boundsClauses = constrainBounds(subBounds, supBounds)
    val baseClauses =
      given Clauses = ctx.addElems(subBounds)
      constrainSub(subBase, supBase)
    return baseClauses.addClauses(boundsClauses)

  // Subtyping of top and bottom types.

  if sub.is[TBot] then
    return Clauses.none

  if sup.is[TTop] then
    return Clauses.none

  // Subtyping of equal variables.

  if sub.is[TVar] && sup.is[TVar] && sub.as[TVar].name == sup.as[TVar].name then
    return Clauses.none

  // Subtyping of fresh variables in constraining mode.

  if mode == Mode.Constrain then
    if sub.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) then
      // TODO: Check and propagate bounds.
      return Clauses(List(Bound(sub.as[TVar].name, Direction.Sub, sup)))

    if sup.is[TVar] && ctx.isTypeVarFresh(sup.as[TVar].name) then
      // TODO: Check and propagate bounds.
      return Clauses(List(Bound(sup.as[TVar].name, Direction.Super, sub)))

  // Subtyping of union and intersection types.

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
      () => constrainSub(subLeft,  sup),
      () => constrainSub(subRight, sup),
    )

  // Subtyping of constrained types.

  if sup.is[TConstrained] then
    return constrainSubConstrainedSup(sub, sup.as[TConstrained])

  if sub.is[TConstrained] then
    return constrainSubConstrainedSub(sub.as[TConstrained], sup)

  // Subtyping of rigid variables or fresh variables in checking mode.

  if sub.is[TVar] && sup.is[TVar] && sub.as[TVar] == sup.as[TVar] then
    // TODO: If two variables, check both bounds, or the correct ones ?
    return Clauses.none

  if sub.is[TVar] then
    val upperBound = ctx.getVarUpperBound(sub.as[TVar].name)
    return constrainSub(upperBound, sup)

  if sup.is[TVar] then
    val lowerBound = ctx.getVarLowerBound(sup.as[TVar].name)
    return constrainSub(sub, lowerBound)

  // Subtyping of lambda types.

  if sub.is[TLam] && sup.is[TLam] then
    return constrainSubLam(sub.as[TLam], sup.as[TLam])

  throw TypeError()

/** Constrain a constrained type to be a subtype of another type. */
def constrainSubConstrainedSub(sub: TConstrained, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  val subVars = sub.vars.map(newFreshVar(_))
  given Clauses = ctx.addElems(subVars, sub.bounds)
  val test = constrainSub(sub.base, sup)
  // TODO: Correctly handle escaping.
  Clauses(subVars).addElems(sub.bounds).addClauses(test)

/** Constrain a type to be a subtype of a constrained type. */
def constrainSubConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Clauses, mode: Mode): Clauses =
  val supVars = sup.vars.map(newRigidVar(_))
  given Clauses = ctx.addElems(supVars, sup.bounds)
  val test = constrainSub(sub, sup.base)
  // TODO: Correctly handle escaping.
  Clauses(supVars).addElems(sup.bounds).addClauses(test)

/** Constrain a lambda type to be a subtype of another lambda type. */
def constrainSubLam(sub: TLam, sup: TLam)(using ctx: Clauses, mode: Mode): Clauses =
  val paramClauses = constrainSub(sup.param, sub.param)
  val retClauses   = constrainSub(sub.ret,   sup.ret)
  return paramClauses.addClauses(retClauses)

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def constrainBounds(subs: List[Bound], sups: List[Bound])(using ctx: Clauses, mode: Mode): Clauses =
  sups
    .foldRight(Clauses.none)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.name, sup.dir)
      val subType = subTypes.mergeMany(sup.dir)
      constrainSub(subType, sup.type_)
    )

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
  def checkBoundSatisfied(bound: Bound): Boolean =
    val var_ = TVar(bound.name)
    bound.dir match
      case Direction.Sub =>
        given Clauses = ctx
        checkSub(var_, bound.type_)
      case Direction.Super =>
        given Clauses = ctx
        checkSub(bound.type_, var_)
