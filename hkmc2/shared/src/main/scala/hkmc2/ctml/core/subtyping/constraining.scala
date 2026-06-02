package hkmc2.ctml.core.subtyping

import scala.collection.immutable.Set as Set

import hkmc2.ctml.config.*
import hkmc2.ctml.core.{filterVarDir, is, isSubClass}
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.inference.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

/** Constrain a set clauses to hold in the context. */
def constrainClauses(clauses: Clauses)(using ctx: Context, mode: ConstraintMode): Clauses =
  clauses.elems.foldRight(Clauses.empty)((clause, clauses) => ctx.seqUnit(constrainClause(clause), clauses))

/** Constrain a clause to hold in the context. */
def constrainClause(clause: Clause)(using ctx: Context, mode: ConstraintMode): Clauses =
  clause match
    case decl: TermVarDecl =>
      Clauses.single(decl)
    case decl: ClassDecl =>
      Clauses.single(decl)
    case decl: TypeVarDecl =>
      Clauses.single(decl)
    case Bound(var_, dir, type_) =>
      subtypeDir(TVar(var_), type_, dir)

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(left: Type, right: Type, dir: Direction)(using ctx: Context, mode: ConstraintMode): Clauses =
  dir match
    case Direction.Sub =>
      subtype(left, right)
    case Direction.Super =>
      subtype(right, left)

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: Clauses)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: Context, mode: ConstraintMode): Clauses =
  try
    subtypeWithDebug(subtypeCache)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `constrainSub` with query cache. */
def subtypeCache(sub: Type, sup: Type)(using ctx: Context, mode: ConstraintMode): Clauses =
  if ctx.cache.check(sub, sup) then
    return Clauses.empty

  given Context = ctx.mapCache(_.add(sub, sup))
  subtypeImpl(sub, sup)

/** Implementation of `constrainSub`. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: Context, mode: ConstraintMode): Clauses =

  // Handle the reflexion case.

  if sub == sup then
    debug(s"REFLEXION ${sub} ${sup}")
    return Clauses.empty

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

  // Subtype of equal type variables, independently of their kind.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub == sup =>
      return Clauses.empty
    case _ =>

  // Subtyping of flexible type variables in simplification mode or rigid variables in
  // reconstruction mode.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub.isFlexMode && sup.isFlexMode =>
      return subtypeFlexVars(sub, sup)
    case (TVar(sub), _) if sub.isFlexMode =>
      return subtypeFlexVar(sub, sup, Direction.Sub)
    case (_, TVar(sup)) if sup.isFlexMode =>
      return subtypeFlexVar(sup, sub, Direction.Super)
    case (_, _) =>

  // Subtyping of union and intersection types.

  sub.splitUnion(Polarity.Negative) match
    case Some(subLeft, subRight) =>
      return ctx.all(
        subtype(subLeft,  sup),
        subtype(subRight, sup),
      )
    case _ =>

  sup.splitUnion(Polarity.Positive) match
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

  sup.splitInter(Polarity.Positive) match
    case Some(supLeft, supRight) =>
      return ctx.all(
        subtype(sub, supLeft),
        subtype(sub, supRight),
      )
    case _ =>

  sub.splitInter(Polarity.Negative) match
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
    case (TVar(sub), TVar(sup)) if sub.isRigidMode && sup.isRigidMode =>
      return subtypeRigidVars(sub, sup)
    case (TVar(sub), _) if sub.isRigidMode =>
      return subtype(sub.upperBound, sup)
    case (_, TVar(sup)) if sup.isRigidMode =>
      return subtype(sub, sup.lowerBound)
    case (_, _) =>

  // Subtyping of constrained types.

  // The right constrained type comes first so that the left constraint (which must be solvable)
  // may be solved using the assumptions of the right constraint (which may not be solvable).

  sup match
    case sup: TConstrained =>
      return subtypeConstrainedSup(sup, sub)
    case _ =>

  sub match
    case sub: TConstrained =>
      return subtypeConstrainedSub(sub, sup)
    case _ =>

  // Subtyping of universal types.

  sup match
    case sup: TUniv =>
      return subtypeUnivSup(sub, sup)
    case _ =>

  sub match
    case sub: TUniv =>
      return subtypeUnivSub(sub, sup)
    case _ =>

  // Subtyping of class type variables.

  (sub, sup) match
    case (TClass(sub), TClass(sup)) if sub.isSubClass(sup) =>
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

  mode match
    // Accept any subtyping constraint in incoherent reconstruction mode.
    case ConstraintMode.Reconstruct if !config.reconstructCoherence =>
      Clauses.empty
    // Raise an error in constraint solving or coherent reconstruction mode.
    case _ =>
      throw TypeError()

// Flexible type variables (or rigid variables in reconstruction mode).

/** Constrain a type variable to be subtype of another type variable. */
def subtypeFlexVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, mode: ConstraintMode): Clauses =
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      Clauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      val y = subtype(TVar(sub), sup.upperBound)
      val supLowerBound = join(TVar(sub), sup.lowerBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, y.concat(Clauses(List(Bound(sup, Direction.Super, supLowerBound)))))
    case (Order.Greater, _) =>
      val x = subtype(sub.lowerBound, TVar(sup))
      val subUpperBound = meet(TVar(sup), sub.upperBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, x.concat(Clauses(List(Bound(sub, Direction.Sub, subUpperBound)))))

/** Constrain a type variable to be subtype or supertype of another type. */
def subtypeFlexVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: Context, mode: ConstraintMode): Clauses =
  // val (extrudedType, outs) = type_.extrude(var_.level, dir.rightPol)
  val (extrudedType, outs) = (type_, Clauses.empty)

  val bound = var_.bound(using ctx.extend(outs))(dir)
  val oppositeBound = var_.bound(using ctx.extend(outs))(!dir)
  val clauses = subtypeDirSeq(oppositeBound, extrudedType, dir, outs)
  if checkSubtypeDir(bound, extrudedType, dir)(using ctx.extend(clauses)) then
    // Do not return a new bound if it is already satisfied in the context.
    clauses
  else
    val newBound = combine(bound, extrudedType, dir)(using ctx.extend(clauses))
    Clauses(Bound(var_, dir, newBound) :: clauses.elems)

// Rigid type variables.

def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: Context, mode: ConstraintMode): Clauses =
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      Clauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      subtype(TVar(sub), sup.lowerBound)
    case (Order.Greater, _) =>
      subtype(sub.upperBound, TVar(sup))

/** Constrain a universal type to be a subtype of another type. */
def subtypeUnivSub(sub: TUniv, sup: Type)(using ctx: Context, mode: ConstraintMode): Clauses =
  val (univVars, univBody) = sub.getUnivComponents
  ctx.withSubtypingLevel2((level) =>
    val (instanceBody, cache, outs) = instantiateUniv(level, univVars, univBody, TypeVarKind.Flex)
    subtypeSeq(instanceBody, sup, outs)(using ctx.mapCache((_) => cache), mode)
  )

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: Context, mode: ConstraintMode): Clauses =
  val (univVars, univBody) = sup.getUnivComponents
  ctx.withSubtypingLevel2((level) =>
    val (instanceBody, cache, outs) = instantiateUniv(level, univVars, univBody, TypeVarKind.Rigid)
    subtypeSeq(sub, instanceBody, outs)(using ctx.mapCache((_) => cache), mode)
  )

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(constrained: TConstrained, type_ : Type)(using ctx: Context, mode: ConstraintMode): Clauses =
  val clauses = subtypeConstraint(constrained.constraint)
  subtypeSeq(constrained.body, type_, clauses)

/** Constrain a constrained type to be a supertype of another type. */
def subtypeConstrainedSup(constrained: TConstrained, type_ : Type)(using ctx: Context, mode: ConstraintMode): Clauses =
  val constraintClauses = try
    config.assumptionMode match
      case AssumptionMode.Flexify =>
        subtypeConstraint(constrained.constraint)(using ctx.flexify(), mode)
      case AssumptionMode.Reconstruct =>
        subtypeConstraint(constrained.constraint)(using ctx, ConstraintMode.Reconstruct)
  catch
    case error: TypeError =>
      if config.subtypeAbsurdConstreds then
        // This line is likely too permissive, as the subtype constraining function does not ensure
        // by itself that two types can never be subtype.
        return Clauses.empty
      else
        throw error

  // TODO: While it makes sense to return new variables that may have been created in the constraints,
  // the only case where that happens currently results in infinite recursion.
  val bodyClauses = subtype(type_, constrained.body)(using ctx.extend(constraintClauses), mode)
  Clauses(constraintClauses.typeVarDecls).concat(bodyClauses)

/** Constrain a tuple type to he a subtype of another tuple type. */
def subtypeTuple(sub: TTuple, sup: TTuple)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.all(
    subtype(sub.left,  sup.left),
    subtype(sub.right, sup.right),
  )

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.all(
    subtype(sup.param, sub.param),
    subtype(sub.ret,   sup.ret),
  )

/** Constrain a type application to be a subtype of another typa application. */
def subtypeApp(sub: TApp, sup: TApp)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.all(
    subtype(sub.abs, sup.abs),
    // Arguments are covariant for now.
    subtype(sub.arg, sup.arg),
  )

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: Context, mode: ConstraintMode): Clauses =
  sups
    .foldRight(Clauses.empty)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.var_, sup.dir)
      val subType = subTypes.combineMany(sup.dir)
      subtype(subType, sup.type_)
    )

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: Context): Boolean =
  try
    withCheckingMode(subtype(sub, sup)(using ctx.rigidify(), ConstraintMode.Solve))
  catch
    case _: TypeError =>
      return false

  return true

def checkSubtypeDir(sub: Type, sup: Type, dir: Direction)(using ctx: Context): Boolean =
  dir match
    case Direction.Sub =>
      checkSubtype(sub, sup)
    case Direction.Super =>
      checkSubtype(sup, sub)

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEqual(left: Type, right: Type)(using ctx: Context): Boolean =
  val a =
    checkSubtype(left, right)
  val b =
    checkSubtype(right, left)
  a && b

/** Check if a bound is satisified in the context. */
def checkBound(bound: Bound)(using ctx: Context): Boolean =
  given Context = ctx
  checkSubtypeDir(TVar(bound.var_), bound.type_, bound.dir)

/** Check whether a subtyping constraint is satisfied in the context. */
def checkConstraint(constraint: Constraint)(using ctx: Context): Boolean =
  checkSubtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraint(constraint: Constraint)(using ctx: Context, mode: ConstraintMode): Clauses =
  subtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraintSeq(constraint: Constraint, ins: Clauses)(using ctx: Context, mode: ConstraintMode): Clauses =
  ctx.seqUnit(subtypeConstraint(constraint), ins)

/** Instantiate the quantified variables of a universal type at the given level, using fresh
 *  variables or approximations from the cache. */
def instantiateUniv(level: Int, vars: List[TypeVar], body: Type, kind: TypeVarKind)(using ctx: Context): (Type, SubtypingCache, Clauses) =
  var instanceBody = body
  var cache = ctx.cache
  var outs = Clauses.empty
  for var_ <- vars do
    val decl = ctx.cache.checkUniv(var_, body) match
      case Some(instanceVar) =>
        instanceVar.decl(using ctx)
      case None =>
        val decl = ctx.declFreshVar(level, kind, var_)
        cache = cache.addUniv(var_, body, decl.var_)
        outs = outs.concat(decl.asClauses)
        decl

    instanceBody = instanceBody.substitute(var_, decl.var_)

  (instanceBody, cache, outs)
