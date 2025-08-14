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
def subtype(sub1: Type, sup1: Type)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  val sup = sub1 match
    case TVar(subVar) =>
      sup1.removeCyclicVar(subVar, Direction.Sub)
    case _ =>
      sup1

  val sub = sup1 match
    case TVar(supVar) =>
      sub1.removeCyclicVar(supVar, Direction.Super)
    case _ =>
      sub1

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
        val (supBody, supBounds) = sup.splitConstrainings()
        val bodyClauses = subtype(sub, supBody)
        return Clauses(supBounds).concat(bodyClauses)

      if sub.is[TConstraining] then
        val (subBody, subBounds) = sub.splitConstrainings()
        val bodyClauses = subtype(subBody, sup)
        return Clauses(subBounds).concat(bodyClauses)
    case Mode.Check =>
      if sub.is[TConstraining] || sup.is[TConstraining] then
        val (subBody, subBounds) = sub.splitConstrainings()
        val (supBody, supBounds) = sup.splitConstrainings()
        val boundsClauses = subtypeBounds(subBounds, supBounds)
        val bodyClauses = subtype(subBody, supBody)
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

  splitUnion(sub) match
    case Some(subLeft, subRight) =>
      return ctx.all(
        subtype(subLeft,  sup),
        subtype(subRight, sup),
      )
    case _ =>

  splitUnion(sup) match
    case Some(supLeft, supRight) =>
      return ctx.any(
        subtype(sub, supLeft),
        subtype(sub, supRight),
      )
    case _ =>

  splitInter(sup) match
    case Some(supLeft, supRight) =>
      return ctx.all(
        subtype(sub, supLeft),
        subtype(sub, supRight),
      )
    case _ =>

  splitInter(sub) match
    case Some(subLeft, subRight) =>
      return ctx.any(
        subtype(subLeft,  sup),
        subtype(subRight, sup),
      )
    case _ =>

  // Subtyping of universal types.

  sup match
    case supUniv: TUniv =>
      return subtypeUnivSup(sub, supUniv)
    case _ =>

  sub match
    case subUniv: TUniv =>
      return subtypeUnivSub(subUniv, sup)
    case _ =>

  // Subtyping of constrained types.

  sup match
    case supConstrained: TConstrained =>
      return subtypeConstrainedSup(sub, supConstrained)
    case _ =>

  sub match
    case subConstrained: TConstrained =>
      return subtypeConstrainedSub(subConstrained, sup)
    case _ =>

  // Subtyping of rigid variables or fresh variables in checking mode.

  (sub, sup) match
    case (subVar: TVar, supVar: TVar) =>
      return subtypeRigidVars(subVar.var_, supVar.var_)
    case (subVar: TVar, _) =>
      val upperBound = ctx.getVarUpperBound(subVar.var_)
      return subtype(upperBound, sup)
    case (_, supVar: TVar) =>
      val lowerBound = ctx.getVarLowerBound(supVar.var_)
      return subtype(sub, lowerBound)
    case (_, _) =>

  // Subtyping of tuple types.

  (sub, sup) match
    case (subTuple: TTuple, supTuple: TTuple) =>
      return subtypeTuple(subTuple, supTuple)
    case _ =>

  // Subtyping of lambda types.

  (sub, sup) match
    case (subLam: TLam, supLam: TLam) =>
      return subtypeLam(subLam, supLam)
    case _ =>

  // Subtyping of type applications.

  (sub, sup) match
    case (subApp: TApp, supApp: TApp) =>
      return subtypeApp(subApp, supApp)
    case _ =>

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

/** Constrain a universal type to be a subtype of another type. */
def subtypeUnivSub(sub: TUniv, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  val freshSub = sub.freshen()
  subtypeSeq(freshSub.body, sup, declFreshVar(freshSub.var_).asClauses)

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: Context, mode: Mode): Clauses =
  subtypeSeq(sub, sup.body, declRigidVar(sup.var_).asClauses)

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(sub: TConstrained, sup: Type)(using ctx: Context, mode: Mode): Clauses =
  val outs = sub.bounds.foldRight(Clauses.empty)(subtypeBoundSeq)
  subtypeSeq(sub.body, sup, outs)

/** Constrain a type to be a subtype of a constrained type. */
def subtypeConstrainedSup(sub: Type, sup: TConstrained)(using ctx: Context, mode: Mode): Clauses =
  val outs = sup.bounds.foldRight(Clauses.empty)(subtypeBoundSeq)
  subtypeSeq(sub, sup.body, outs)

/** Constrain a tuple type to he a subtype of another tuple type. */
def subtypeTuple(sub: TTuple, sup: TTuple)(using ctx: Context, mode: Mode): Clauses =
  val leftClauses = subtype(sub.left, sup.left)
  subtypeSeq(sub.right, sup.right, leftClauses)

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Context, mode: Mode): Clauses =
  val paramClauses = subtype(sup.param, sub.param)
  subtypeSeq(sub.ret, sup.ret, paramClauses)

/** Constrain a typa application to be a subtype of another typa application. */
def subtypeApp(sub: TApp, sup: TApp)(using ctx: Context, mode: Mode): Clauses =
  val absClauses = subtype(sup.abs, sub.abs)
  // The argument is invariant.
  val subClauses = subtypeSeq(sub.arg, sup.arg, absClauses)
  subtypeSeq(sup.arg, sub.arg, absClauses)
  subClauses

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
