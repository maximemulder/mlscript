package hkmc2.ctml.core.subtyping

import scala.collection.immutable.Set as Set

import hkmc2.ctml.core.{filterVarDir, is, isSubClass}
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.type_.impls.substitute.substitute

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: Clauses)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(left: Type, right: Type, dir: Direction)(using ctx: Context, cache: SubtypingCache): Clauses =
  dir match
    case Direction.Sub =>
      subtype(left, right)
    case Direction.Super =>
      subtype(right, left)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: Context, cache: SubtypingCache): Clauses =
  try
    subtypeWithDebug(subtypeCache)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `constrainSub` with query cache. */
def subtypeCache(sub: Type, sup: Type)(using ctx: Context, cache: SubtypingCache): Clauses =
  if cache.check(sub, sup) then
    return Clauses.empty

  given SubtypingCache = cache.add(sub, sup)
  subtypeImpl(sub, sup)

/** Implementation of `constrainSub`. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: Context, cache: SubtypingCache): Clauses =

  // Normalize negation types.

  sub match
    case TNeg(sub) =>
      sub.negateStep() match
        case Some(sub) =>
          return subtype(sub, sup)
        case _ =>
    case _ =>

  sup match
    case TNeg(sup) =>
      sup.negateStep() match
        case Some(sup) =>
          return subtype(sub, sup)
        case _ =>
    case _ =>

  // Subtype negation types

  (sub, sup) match
    case (TNeg(sub), TNeg(sup)) =>
      return subtype(sup, sub)
    case _ =>

  sup match
    case TNeg(sup) if areDisjointConstructors(sub, sup) =>
      return Clauses.empty
    case _ =>

  // Subtyping of constraining types.

  if sub.is[TConstraining] && sup.is[TConstraining] then
    val (subBody, subConstraints) = sub.getConstrainingComponents
    val (supBody, supConstraints) = sup.getConstrainingComponents
    val subClauses = subConstraints.foldLeft(Clauses.empty)((clauses, constraint) =>
      ctx.seqUnit(subtypeConstraint(constraint), clauses)
    )
    val supClauses = supConstraints.foldLeft(Clauses.empty)((clauses, constraint) =>
      ctx.seqUnit(subtypeConstraint(constraint), clauses)
    )

    val boundsClauses = subtypeBounds(subClauses.bounds, supClauses.bounds)
    val bodyClauses = subtype(subBody, supBody)
    return Clauses.empty

  sub match
    case TConstraining(subBody, subConstraint) =>
      val bodyClauses = subtype(subBody, sup)
      return subtypeConstraintSeq(subConstraint, bodyClauses)
    case _ =>
  sup match
    case TConstraining(supBody, supConstraint) =>
      val bodyClauses = subtype(sub, supBody)
      return subtypeConstraintSeq(supConstraint, bodyClauses)
    case _ =>

  // Subtyping of top and bottom types.

  if sub.is[TBot] then
    return Clauses.empty

  if sup.is[TTop] then
    return Clauses.empty

  // Subtyping of flexible type variables.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub.isFlex && sup.isFlex =>
      return subtypeFlexVars(sub, sup)
    case _ =>

  sub match
    case TVar(sub) if sub.isFlex =>
      return subtypeFlexVar(sub, sup, Direction.Sub)
    case _ =>

  sup match
    case TVar(sup) if sup.isFlex =>
      return subtypeFlexVar(sup, sub, Direction.Super)
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
      return joinMerge(supLeft, supRight) match
        case Some(sup) =>
          subtype(sub, sup)
        case None =>
          ctx.any(
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
      return meetMerge(subLeft, subRight) match
        case Some(sub) =>
          subtype(sub, sup)
        case None =>
          ctx.any(
            subtype(subLeft,  sup),
            subtype(subRight, sup),
          )
    case _ =>

  // Subtyping of rigid type variables.

  (sub, sup) match
    case (sub: TVar, sup: TVar) if sub.isRigidVar && sup.isRigidVar =>
      return subtypeRigidVars(sub.var_, sup.var_)
    case (sub: TVar, _) if sub.isRigidVar =>
      return subtype(sub.var_.upperBound, sup)
    case (_, sup: TVar) if sup.isRigidVar =>
      return subtype(sub, sup.var_.lowerBound)
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
      return subtypeConstrainedSub(sub, sup)
    case _ =>

  sup match
    case sup: TConstrained =>
      return subtypeConstrainedSup(sup, sub)
    case _ =>

  // Subtyping of class type variables.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub.isClass && sup.isClass && sub.isSubClass(sup) =>
      return Clauses.empty
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

// Flexible type variables.

/** Constrain a type variable to be subtype of another type variable. */
def subtypeFlexVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.compareVarLevels(sub, sup) match
    // If both variables are equal then they are subtype.
    case Order.Equal =>
      Clauses.empty
    case _ =>
      val subUpperBound = meet(TVar(sup), sub.upperBound)
      val supLowerBound = join(TVar(sub), sup.lowerBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, Clauses(List(Bound(sub, Direction.Sub, subUpperBound), Bound(sup, Direction.Super, supLowerBound))))

/** Constrain a type variable to be subtype or supertype of another type. */
def subtypeFlexVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: Context, cache: SubtypingCache): Clauses =
  val oppositeBoundType = ctx.getVarBound(var_, dir.invert())
  val clauses = subtypeDir(oppositeBoundType, type_, dir)
  val boundType = var_.bound(dir)
  if checkSubtypeDir(boundType, type_, dir)(using ctx.extend(clauses)) then
    // Do not return a new bound if it is already satisfied in the context.
    clauses
  else
    val boundCombinedType = combine(boundType, type_, dir)
    Clauses(Bound(var_, dir, boundCombinedType) :: clauses.elems)

// Rigid type variables.

def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.compareVarLevels(sub, sup) match
    // If both variables are equal then they are subtype.
    case Order.Equal =>
      return Clauses.empty
    case Order.Lesser =>
      subtype(TVar(sub), sup.lowerBound)
    case Order.Greater =>
      subtype(sub.upperBound, TVar(sup))

/** Constrain a universal type to be a subtype of another type. */
def subtypeUnivSub(sub: TUniv, sup: Type)(using ctx: Context, cache: SubtypingCache): Clauses =
  val freshDecl = declFreshFlexVar(Some(sub.var_))
  val freshBody = sub.body.substitute(sub.var_, freshDecl.var_)
  subtypeSeq(freshBody, sup, freshDecl.asClauses)

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: Context, cache: SubtypingCache): Clauses =
  val freshDecl = declFreshRigidVar(Some(sup.var_))
  val freshBody = sup.body.substitute(sup.var_, freshDecl.var_)
  subtypeSeq(sub, freshBody, freshDecl.asClauses)

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(constrained: TConstrained, type_ : Type)(using ctx: Context, cache: SubtypingCache): Clauses =
  val clauses = subtypeConstraint(constrained.constraint)
  subtypeSeq(constrained.body, type_, clauses)

/** Constrain a constrained type to be a supertype of another type. */
def subtypeConstrainedSup(constrained: TConstrained, type_ : Type)(using ctx: Context, cache: SubtypingCache): Clauses =
  val vars = constrained.constraint.getVars()
  val newCtx = ctx.flexify(vars)
  val clauses = subtypeConstraint(constrained.constraint)(using newCtx, cache)
  subtypeSeq(type_, constrained.body, clauses)

/** Constrain a tuple type to he a subtype of another tuple type. */
def subtypeTuple(sub: TTuple, sup: TTuple)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.all(
    subtype(sub.left,  sup.left),
    subtype(sub.right, sup.right),
  )

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.all(
    subtype(sup.param, sub.param),
    subtype(sub.ret,   sup.ret),
  )

/** Constrain a type application to be a subtype of another typa application. */
def subtypeApp(sub: TApp, sup: TApp)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.all(
    subtype(sub.abs, sup.abs),
    // Arguments are covariant for now.
    subtype(sub.arg, sup.arg),
  )

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: Context, cache: SubtypingCache): Clauses =
  sups
    .foldRight(Clauses.empty)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.var_, sup.dir)
      val subType = subTypes.combineMany(sup.dir)
      subtype(subType, sup.type_)
    )

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: Context, cache: SubtypingCache): Boolean =
  given Context = ctx.freeze()
  try
    withCheckingMode(subtype(sub, sup))
  catch
    case _: TypeError =>
      return false

  return true

def checkSubtypeDir(sub: Type, sup: Type, dir: Direction)(using ctx: Context, cache: SubtypingCache): Boolean =
  dir match
    case Direction.Sub =>
      checkSubtype(sub, sup)
    case Direction.Super =>
      checkSubtype(sup, sub)

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEqual(left: Type, right: Type)(using ctx: Context): Boolean =
  val a =
    given SubtypingCache = SubtypingCache()
    checkSubtype(left, right)
  val b =
    given SubtypingCache = SubtypingCache()
    checkSubtype(right, left)
  a && b

// TODO: This should not be a method.
extension (ctx: Context)
  /** Check if a bound is satisified in the context. */
  def checkBoundSatisfied(bound: Bound): Boolean =
    bound.dir match
      case Direction.Sub =>
        given Context = ctx
        given SubtypingCache = SubtypingCache()
        checkSubtype(TVar(bound.var_), bound.type_)
      case Direction.Super =>
        given Context = ctx
        given SubtypingCache = SubtypingCache()
        checkSubtype(bound.type_, TVar(bound.var_))

/** Check whether a subtyping constraint is satisfied in the context. */
def checkConstraint(constraint: Constraint)(using ctx: Context): Boolean =
  given SubtypingCache = SubtypingCache()
  checkSubtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraint(constraint: Constraint)(using ctx: Context, cache: SubtypingCache): Clauses =
  subtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraintSeq(constraint: Constraint, ins: Clauses)(using ctx: Context, cache: SubtypingCache): Clauses =
  ctx.seqUnit(subtypeConstraint(constraint), ins)

