package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(sub: Type, sup: Type, dir: Direction)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  dir match
    case Direction.Sub =>
      subtype(sub, sup)
    case Direction.Super =>
      subtype(sup, sub)

/** Constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  seq(() => subtype(sub, sup), ins)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  try
    subtypeWithDebug(subtypeImpl)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `constrainSub`. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  // Subtyping of constraining types.

  mode match
    case Mode.Constrain =>
      if sup.is[TConstraining] then
        val (supBase, supBounds) = sup.splitConstrainings()
        val baseClauses = subtype(sub, supBase)
        return Clauses(supBounds).concat(baseClauses)

      if sub.is[TConstraining] then
        val (subBase, subBounds) = sub.splitConstrainings()
        val baseClauses = subtype(subBase, sup)
        return Clauses(subBounds).concat(baseClauses)
    case Mode.Check =>
      if sub.is[TConstraining] || sup.is[TConstraining] then
        val (subBase, subBounds) = sub.splitConstrainings()
        val (supBase, supBounds) = sup.splitConstrainings()
        val boundsClauses = subtypeBounds(subBounds, supBounds)
        val baseClauses = subtype(subBase, supBase)
        return Clauses.none

  // Subtyping of top and bottom types.

  if sub.is[TBot] then
    return Clauses.none

  if sup.is[TTop] then
    return Clauses.none

  // Subtyping of fresh variables in constraining mode.

  if mode == Mode.Constrain then
    if sub.isFreshVar && sup.isFreshVar then
      return subtypeFreshVars(sub.as[TVar].var_, sup.as[TVar].var_)

    if sub.isFreshVar then
      return subtypeFreshVarSub(sub.as[TVar].var_, sup)

    if sup.isFreshVar then
      return subtypeFreshVarSup(sup.as[TVar].var_, sub)

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
    return subtypeRigidVars(sub.as[TVar].var_, sup.as[TVar].var_)

  if sub.is[TVar] then
    val upperBound = ctx.getVarUpperBound(sub.as[TVar].var_)
    return subtype(upperBound, sup)

  if sup.is[TVar] then
    val lowerBound = ctx.getVarLowerBound(sup.as[TVar].var_)
    return subtype(sub, lowerBound)

  // Subtyping of lambda types.

  if sub.is[TLam] && sup.is[TLam] then
    return subtypeLam(sub.as[TLam], sup.as[TLam])

  throw TypeError()

// Fresh variables.

/** Constrain a type variable to be subtype of another type variable. */
def subtypeFreshVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, mode: Mode): Clauses =
  // If both variables are equal then they are subtype.
  if sub == sup then
    return Clauses.none

  var lowest = ctx.compareVarLevels(sub, sup)
  lowest match
    case Left(()) =>
      subtypeFreshVarSub(sub, TVar(sup))
    case Right(()) =>
      subtypeFreshVarSup(sup, TVar(sub))

/** Constrain a type variable to be subtype of a type. */
def subtypeFreshVarSub(var_ : TypeVar, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  val upperBound = Bound(var_, Direction.Sub, sup)
  val lowerBound = ctx.getVarLowerBound(var_)
  subtypeSeq(lowerBound, sup, upperBound.asClauses)

/** Constrain a type variable to be supertype of a type. */
def subtypeFreshVarSup(var_ : TypeVar, sub: Type)(using ctx: Context, mode: Mode): Clauses =
  val lowerBound = Bound(var_, Direction.Super, sub)
  val upperBound = ctx.getVarUpperBound(var_)
  subtypeSeq(sub, upperBound, lowerBound.asClauses)

// Rigid variables.

def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, mode: Mode): Clauses =
  // If both variables are equal then they are subtype.
  if sub.name == sup.name then
    return Clauses.none

  var lowest = ctx.compareVarLevels(sub, sup)
  lowest match
    case Left(()) =>
      val upperBound = ctx.getVarUpperBound(sub)
      subtype(upperBound, TVar(sup))
    case Right(()) =>
      val lowerBound = ctx.getVarLowerBound(sup)
      subtype(TVar(sub), lowerBound)

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(sub: TConstrained, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  val subVars = sub.vars.map(newFreshVar(_))
  /* given Context = ctx.extend(subVars, sub.bounds)
  val test = subtype(sub.base, sup)
  // TODO: Correctly handle escaping.
  Clauses(subVars).concat(test) */
  // TODO: Do not add the bounds directly to the clauses but constrain.
  val outs = sub.bounds.foldRight(Clauses.none)((bound, outs) => seq(() => constrainBound(bound), outs))
  subtypeSeq(sub.base, sup, Clauses(subVars).concat(outs))

/** Constrain a type to be a subtype of a constrained type. */
def subtypeConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Context, mode: Mode): Clauses =
  val supVars = sup.vars.map(newRigidVar(_))
  /* given Context = ctx.extend(supVars, sup.bounds)
  val test = subtype(sub, sup.base)
  // TODO: Correctly handle escaping.
  Clauses(supVars).concat(test) */
  // TODO: Do not add the bounds directly to the clauses but constrain.
  val outs = sup.bounds.foldRight(Clauses.none)((bound, outs) => seq(() => constrainBound(bound), outs))
  subtypeSeq(sub, sup.base, Clauses(supVars).concat(outs))

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Context, mode: Mode): Clauses =
  val paramClauses = subtype(sup.param, sub.param)
  subtypeSeq(sub.ret, sup.ret, paramClauses)

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: Context, mode: Mode): Clauses =
  sups
    .foldRight(Clauses.none)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.var_, sup.dir)
      val subType = subTypes.combineMany(sup.dir)
      subtype(subType, sup.type_)
    )

def constrainBound(bound: Bound)(using ctx: Context, mode: Mode): Clauses =
  subtypeDir(TVar(bound.var_), bound.type_, bound.dir)

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: Context): Boolean =
  given Mode = Mode.Check
  try
    subtype(sub, sup)
  catch
    case _: TypeError =>
      return false

  return true

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEqual(left: Type, right: Type)(using ctx: Context): Boolean =
  checkSubtype(left, right) && checkSubtype(right, left)

extension (ctx: Context)
  /** Check if a bound is satisified in the context. */
  def checkBoundSatisfied(bound: Bound): Boolean =
    bound.dir match
      case Direction.Sub =>
        given Context = ctx
        checkSubtype(TVar(bound.var_), bound.type_)
      case Direction.Super =>
        given Context = ctx
        checkSubtype(bound.type_, TVar(bound.var_))
