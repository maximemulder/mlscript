package hkmc2.ctml.core

import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(sub: Type, sup: Type, dir: Direction)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  dir match
    case Direction.Sub =>
      subtype(sub, sup)
    case Direction.Super =>
      subtype(sup, sub)

/** Constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  given Clauses = ctx.addClauses(ins)
  val outs = subtype(sub, sup)
  ins.addClauses(outs)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  try
    subtypeImpl(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `constrainSub`. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: Clauses, mode: Mode = Mode.Constrain): Clauses =
  // Subtyping of constraining types.

  mode match
    case Mode.Constrain =>
      if sup.is[TConstraining] then
        val (supBase, supBounds) = sup.splitConstrainings()
        val baseClauses = subtype(sub, supBase)
        return Clauses(supBounds).addClauses(baseClauses)

      if sub.is[TConstraining] then
        val (subBase, subBounds) = sub.splitConstrainings()
        val baseClauses = subtype(subBase, sup)
        return Clauses(subBounds).addClauses(baseClauses)
    case Mode.Check =>
      if sub.is[TConstraining] || sup.is[TConstraining] then
        val (subBase, subBounds) = sub.splitConstrainings()
        val (supBase, supBounds) = sup.splitConstrainings()
        val boundsClauses = subtypeBounds(subBounds, supBounds)
        val baseClauses =
          given Clauses = ctx.addElems(subBounds)
          subtype(subBase, supBase)
        return Clauses.none

  // Subtyping of top and bottom types.

  if sub.is[TBot] then
    return Clauses.none

  if sup.is[TTop] then
    return Clauses.none

  // Subtyping of fresh variables in constraining mode.

  if mode == Mode.Constrain then
    if sub.is[TVar] && sup.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) && ctx.isTypeVarFresh(sup.as[TVar].name) then
      return subtypeFreshVars(sub.as[TVar], sup.as[TVar])

    if sub.is[TVar] && ctx.isTypeVarFresh(sub.as[TVar].name) then
      return subtypeFreshVarSub(sub.as[TVar], sup)

    if sup.is[TVar] && ctx.isTypeVarFresh(sup.as[TVar].name) then
      return subtypeFreshVarSup(sub, sup.as[TVar])

  // Subtyping of union and intersection types.

  var subUnionSplit = splitUnion(sub)
  if subUnionSplit.isDefined then
    val (subLeft, subRight) = subUnionSplit.get
    return ctx.all(
      () => subtype(subLeft,  sup),
      () => subtype(subRight, sup),
    )

  var supUnionSplit = splitUnion(sup)
  if supUnionSplit.isDefined then
    val (supLeft, supRight) = supUnionSplit.get
    return ctx.any(
      () => subtype(sub, supLeft),
      () => subtype(sub, supRight),
    )

  var supInterSplit = splitInter(sup)
  if supInterSplit.isDefined then
    val (supLeft, supRight) = supInterSplit.get
    return ctx.all(
      () => subtype(sub, supLeft),
      () => subtype(sub, supRight),
    )

  var subInterSplit = splitInter(sub)
  if subInterSplit.isDefined then
    val (subLeft, subRight) = subInterSplit.get
    return ctx.any(
      () => subtype(subLeft,  sup),
      () => subtype(subRight, sup),
    )

  // Subtyping of constrained types.

  if sup.is[TConstrained] then
    return subtypeConstrainedSup(sub, sup.as[TConstrained])

  if sub.is[TConstrained] then
    return subtypeConstrainedSub(sub.as[TConstrained], sup)

  // Subtyping of rigid variables or fresh variables in checking mode.

  if sub.is[TVar] && sup.is[TVar] then
    return subtypeRigidVars(sub.as[TVar], sup.as[TVar])

  if sub.is[TVar] then
    val upperBound = ctx.getVarUpperBound(sub.as[TVar].name)
    return subtype(upperBound, sup)

  if sup.is[TVar] then
    val lowerBound = ctx.getVarLowerBound(sup.as[TVar].name)
    return subtype(sub, lowerBound)

  // Subtyping of lambda types.

  if sub.is[TLam] && sup.is[TLam] then
    return subtypeLam(sub.as[TLam], sup.as[TLam])

  throw TypeError()

def subtypeFreshVars(sub: TVar, sup: TVar)(using ctx: Clauses, mode: Mode): Clauses =
  // If both variables are equal then they are subtype.
  if sub.name == sup.name then
    return Clauses.none

  var lowest = ctx.compareVarLevels(sub.name, sup.name)
  lowest match
    case Left(()) =>
      subtypeFreshVarSub(sub, sup)
    case Right(()) =>
      subtypeFreshVarSup(sub, sup)

def subtypeFreshVarSub(sub: TVar, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  val varName = sub.as[TVar].name
  val upperBound = Bound(varName, Direction.Sub, sup)
  val lowerBound = ctx.getVarLowerBound(varName)
  subtypeSeq(lowerBound, sup, upperBound.asClauses)

def subtypeFreshVarSup(sub: Type, sup: TVar)(using ctx: Clauses, mode: Mode): Clauses =
  val varName = sup.as[TVar].name
  val lowerBound = Bound(varName, Direction.Super, sub)
  val upperBound = ctx.getVarUpperBound(varName)
  subtypeSeq(sub, upperBound, lowerBound.asClauses)

def subtypeRigidVars(sub: TVar, sup: TVar)(using ctx: Clauses, mode: Mode): Clauses =
  // If both variables are equal then they are subtype.
  if sub.name == sup.name then
    return Clauses.none

  var lowest = ctx.compareVarLevels(sub.name, sup.name)
  lowest match
    case Left(()) =>
      val upperBound = ctx.getVarUpperBound(sub.name)
      subtype(upperBound, sup)
    case Right(()) =>
      val lowerBound = ctx.getVarLowerBound(sup.name)
      subtype(sub, lowerBound)

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(sub: TConstrained, sup: Type)(using ctx: Clauses, mode: Mode): Clauses =
  val subVars = sub.vars.map(newFreshVar(_))
  given Clauses = ctx.addElems(subVars, sub.bounds)
  val test = subtype(sub.base, sup)
  // TODO: Correctly handle escaping.
  Clauses(subVars).addElems(sub.bounds).addClauses(test)

/** Constrain a type to be a subtype of a constrained type. */
def subtypeConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Clauses, mode: Mode): Clauses =
  val supVars = sup.vars.map(newRigidVar(_))
  given Clauses = ctx.addElems(supVars, sup.bounds)
  val test = subtype(sub, sup.base)
  // TODO: Correctly handle escaping.
  Clauses(supVars).addElems(sup.bounds).addClauses(test)

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Clauses, mode: Mode): Clauses =
  val paramClauses = subtype(sup.param, sub.param)
  subtypeSeq(sub.ret, sup.ret, paramClauses)

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: Clauses, mode: Mode): Clauses =
  sups
    .foldRight(Clauses.none)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.name, sup.dir)
      val subType = subTypes.mergeMany(sup.dir)
      subtype(subType, sup.type_)
    )

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: Clauses): Boolean =
  given Mode = Mode.Check
  try
    subtype(sub, sup)
  catch
    case _: TypeError =>
      return false

  return true

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEqual(left: Type, right: Type)(using ctx: Clauses): Boolean =
  checkSubtype(left, right) && checkSubtype(right, left)

extension (ctx: Clauses)
  /** Check if a bound is satisified in the context. */
  def checkBoundSatisfied(bound: Bound): Boolean =
    val var_ = TVar(bound.name)
    bound.dir match
      case Direction.Sub =>
        given Clauses = ctx
        checkSubtype(var_, bound.type_)
      case Direction.Super =>
        given Clauses = ctx
        checkSubtype(bound.type_, var_)
