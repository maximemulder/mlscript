package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Sequentially constrain a type variable bound in a context. */
def subtypeBoundSeq(bound: Bound, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtypeBound(bound), ins)

/** Constrain a type variable bound to be satisfied. */
def subtypeBound(bound: Bound)(using ctx: Context, mode: Mode): Clauses =
  subtypeDir(TVar(bound.var_), bound.type_, bound.dir)

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(left: Type, right: Type, dir: Direction)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  dir match
    case Direction.Sub =>
      subtype(left, right)
    case Direction.Super =>
      subtype(right, left)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  try
    subtypeWithDebug(subtypeImpl)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup, mode))
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
        return Clauses.empty

  // Subtyping of top and bottom types.

  if sub.is[TBot] then
    return Clauses.empty

  if sup.is[TTop] then
    return Clauses.empty

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
      subtype(subLeft,  sup),
      subtype(subRight, sup),
    )

  var supUnionSplit = splitUnion(sup)
  if supUnionSplit.isDefined then
    val (supLeft, supRight) = supUnionSplit.get
    return ctx.any(
      subtype(sub, supLeft),
      subtype(sub, supRight),
    )

  var supInterSplit = splitInter(sup)
  if supInterSplit.isDefined then
    val (supLeft, supRight) = supInterSplit.get
    return ctx.all(
      subtype(sub, supLeft),
      subtype(sub, supRight),
    )

  var subInterSplit = splitInter(sub)
  if subInterSplit.isDefined then
    val (subLeft, subRight) = subInterSplit.get
    return ctx.any(
      subtype(subLeft,  sup),
      subtype(subRight, sup),
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
  ctx.compareVarLevels(sub, sup) match
    // If both variables are equal then they are subtype.
    case Order.Equal =>
      Clauses.empty
    case Order.Lesser =>
      subtypeFreshVarSup(sup, TVar(sub))
    case Order.Greater =>
      subtypeFreshVarSub(sub, TVar(sup))

/** Constrain a type variable to be subtype of a type. */
def subtypeFreshVarSub(var_ : TypeVar, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  subtypeFreshVar(var_, sup, Direction.Sub)

/** Constrain a type variable to be supertype of a type. */
def subtypeFreshVarSup(var_ : TypeVar, sub: Type)(using ctx: Context, mode: Mode): Clauses =
  subtypeFreshVar(var_, sub, Direction.Super)

def subtypeFreshVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: Context, mode: Mode): Clauses =
  val boundType = ctx.getVarBound(var_, dir)
  val boundCombinedType = combine(boundType, type_, dir)
  val bound = Bound(var_, dir, boundCombinedType)
  val oppositeBoundType = ctx.getVarBound(var_, dir.invert())
  subtypeDirSeq(oppositeBoundType, boundCombinedType, dir, bound.asClauses)

// Rigid variables.

def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, mode: Mode): Clauses =
  ctx.compareVarLevels(sub, sup) match
    // If both variables are equal then they are subtype.
    case Order.Equal =>
      return Clauses.empty
    case Order.Lesser =>
      val lowerBound = ctx.getVarLowerBound(sup)
      subtype(TVar(sub), lowerBound)
    case Order.Greater =>
      val upperBound = ctx.getVarUpperBound(sub)
      subtype(upperBound, TVar(sup))

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(sub: TConstrained, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  val freshSub = sub.freshenConstrainedType()
  val outs = freshSub.bounds.foldRight(Clauses(freshSub.vars.map(declFreshVar(_)).asClauses))(subtypeBoundSeq)
  subtypeSeq(freshSub.base, sup, outs)

/** Constrain a type to be a subtype of a constrained type. */
def subtypeConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Context, mode: Mode): Clauses =
  val supDecls = sup.vars.map(declRigidVar(_))
  val outs = sup.bounds.foldRight(Clauses(supDecls))(subtypeBoundSeq)
  subtypeSeq(sub, sup.base, outs)

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Context, mode: Mode): Clauses =
  val paramClauses = subtype(sup.param, sub.param)
  subtypeSeq(sub.ret, sup.ret, paramClauses)

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: Context, mode: Mode): Clauses =
  sups
    .foldRight(Clauses.empty)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.var_, sup.dir)
      val subType = subTypes.combineMany(sup.dir)
      subtype(subType, sup.type_)
    )

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
