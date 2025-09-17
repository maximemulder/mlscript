package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Sequentially constrain a type variable bound in a context. */
def subtypeBoundSeq(bound: Bound, ins: Clauses)(using ctx: Context, mode: Mode = Mode.Constrain): Clauses =
  ctx.seqUnit(subtypeBound(bound), ins)

/** Constrain a type variable bound to be satisfied. */
def subtypeBound(bound: Bound)(using ctx: Context, mode: Mode): Clauses =
  given Context = ctx.flexify(bound.var_)
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

  // Subtyping of flexible type variables.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub.isFlex && sup.isFlex =>
      return subtypeFreshVars(sub, sup)
    case _ =>

  sub match
    case TVar(sub) if sub.isFlex =>
      return subtypeFreshVar(sub, sup, Direction.Sub)
    case _ =>

  sup match
    case TVar(sup) if sup.isFlex =>
      return subtypeFreshVar(sup, sub, Direction.Super)
    case _ =>

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

  // Subtyping of rigid type variables.

  (sub, sup) match
    case (sub: TVar, sup: TVar) =>
      return subtypeRigidVars(sub.var_, sup.var_)
    case (sub: TVar, _) =>
      val upperBound = ctx.getVarUpperBound(sub.var_)
      return subtype(upperBound, sup)
    case (_, sup: TVar) =>
      val lowerBound = ctx.getVarLowerBound(sup.var_)
      return subtype(sub, lowerBound)
    case (_, _) =>

  // Subtyping of universal types.

  sup match
    case sup: TUniv =>
      return subtypeUnivSup(sub, sup)
    case _ =>

  sub match
    case sub: TUniv =>
      return subtypeUnivSub(sub, sup)
    case _ =>

  // Subtyping of constrained types.

  sub match
    case sub: TConstrained =>
      return subtypeConstrained(sub, sup, Direction.Sub)
    case _ =>

  sup match
    case sup: TConstrained =>
      return subtypeConstrained(sup, sub, Direction.Super)
    case _ =>

  // Subtyping of tuple types.

  (sub, sup) match
    case (sub: TTuple, sup: TTuple) =>
      return subtypeTuple(sub, sup)
    case _ =>

  // Subtyping of lambda types.

  (sub, sup) match
    case (sub: TLam, sup: TLam) =>
      return subtypeLam(sub, sup)
    case _ =>

  // Subtyping of type applications.

  (sub, sup) match
    case (sub: TApp, sup: TApp) =>
      return subtypeApp(sub, sup)
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
      subtypeFreshVar(sup, TVar(sub), Direction.Super)
    case Order.Greater =>
      subtypeFreshVar(sub, TVar(sup), Direction.Sub)

/** Constrain a type variable to be subtype or supertype of another type. */
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
  subtypeSeq(freshSub.body, sup, declFlexVar(freshSub.var_).asClauses)

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: Context, mode: Mode): Clauses =
  val freshSup = sup.freshen()
  subtypeSeq(sub, freshSup.body, declRigidVar(freshSup.var_).asClauses)

/** Constrain a constrained type to be a subtype or supertype of another type. */
def subtypeConstrained(constrained: TConstrained, type_ : Type, dir: Direction)(using ctx: Context, mode: Mode): Clauses =
  val clauses = constrained.bounds.foldRight(Clauses.empty)(subtypeBoundSeq)
  subtypeDirSeq(constrained.body, type_, dir, clauses)

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
  // Arguments are covariant for now.
  subtypeSeq(sub.arg, sup.arg, absClauses)

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
  given Context = ctx.freeze()
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
