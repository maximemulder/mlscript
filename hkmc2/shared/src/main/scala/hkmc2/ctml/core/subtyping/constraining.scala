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
def constrainClauses(clauses: SubClauses)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  clauses.elems.foldRight(SubClauses.empty)((clause, clauses) => ctx.seqUnit(constrainClause(clause), clauses))

/** Constrain a clause to hold in the context. */
def constrainClause(clause: SubClause)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  clause match
    case decl: ClassDecl =>
      SubClauses.single(decl)
    case decl: TypeVarDecl =>
      SubClauses.single(decl)
    case Bound(var_, dir, type_) =>
      subtypeDir(TVar(var_), type_, dir)

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: SubClauses)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(left: Type, right: Type, dir: Direction)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  dir match
    case Direction.Sub =>
      subtype(left, right)
    case Direction.Super =>
      subtype(right, left)

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: SubClauses)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  try
    subtypeWithDebug(subtypeCache)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `constrainSub` with query cache. */
def subtypeCache(sub: Type, sup: Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  if ctx.cache.check(sub, sup) then
    return SubClauses.empty

  given SubContext = ctx.mapCache(_.add(sub, sup))
  subtypeImpl(sub, sup)

/** Implementation of `constrainSub`. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =

  // Handle the reflexion case.

  if sub == sup then
    return SubClauses.empty

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
      return SubClauses.empty
    case _ =>

  // Subtyping of constraining types.

  if sub.is[TConstraining] && sup.is[TConstraining] then
    val (subBody, subConstraints) = sub.getConstrainingComponents
    val (supBody, supConstraints) = sup.getConstrainingComponents
    val subClauses = subConstraints.foldLeft(SubClauses.empty)((clauses, constraint) =>
      ctx.seqUnit(subtypeConstraint(constraint), clauses)
    )
    val supClauses = supConstraints.foldLeft(SubClauses.empty)((clauses, constraint) =>
      ctx.seqUnit(subtypeConstraint(constraint), clauses)
    )

    val boundsClauses = subtypeBounds(subClauses.bounds, supClauses.bounds)
    val bodyClauses = subtype(subBody, supBody)
    return SubClauses.empty

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
    return SubClauses.empty

  if sup.is[TTop] then
    return SubClauses.empty

  // Subtype of equal type variables, independently of their kind.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub == sup =>
      return SubClauses.empty
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

  // Expand transparent rigid aliases before structural decomposition, so that rules under the
  // alias (notably right universal introduction) govern the whole judgment.

  // TODO: Investigate this block.

  (sub, sup) match
    case (TVar(sub), _) if sub.isRigidMode && sub.lowerBound == sub.upperBound =>
      return subtype(sub.upperBound, sup)
    case (_, TVar(sup)) if sup.isRigidMode && sup.lowerBound == sup.upperBound =>
      return subtype(sub, sup.lowerBound)
    case (_, _) =>

  // Introduce right universal variables before decomposing the subtype. This keeps a single
  // arbitrary instance in scope for every conjunctive branch of the same judgment.

  sup match
    case sup: TUniv =>
      return subtypeUnivSup(sub, sup)
    case _ =>

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

  sub match
    case sub: TUniv =>
      return subtypeUnivSub(sub, sup)
    case _ =>

  // Subtyping of class type variables.

  (sub, sup) match
    case (TClass(sub), TClass(sup)) if sub.isSubClass(sup) =>
      return SubClauses.empty
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
      SubClauses.empty
    // Raise an error in constraint solving or coherent reconstruction mode.
    case _ =>
      throw TypeError()

// Flexible type variables (or rigid variables in reconstruction mode).

/** Constrain a type variable to be subtype of another type variable. */
def subtypeFlexVars(sub: TypeVar, sup: TypeVar)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      SubClauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      val y = subtype(TVar(sub), sup.upperBound)
      val supLowerBound = join(TVar(sub), sup.lowerBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, y.concat(SubClauses(List(Bound(sup, Direction.Super, supLowerBound)))))
    case (Order.Greater, _) =>
      val x = subtype(sub.lowerBound, TVar(sup))
      val subUpperBound = meet(TVar(sup), sub.upperBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, x.concat(SubClauses(List(Bound(sub, Direction.Sub, subUpperBound)))))

/** Constrain a type variable to be subtype or supertype of another type. */
def subtypeFlexVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val (extrudedType, outs) = if config.extrudeVar then
    type_.extrude(var_.level, dir.rightPol)
  else
    (type_, SubClauses.empty)

  val bound = var_.bound(using ctx.extend(outs))(dir)
  val oppositeBound = var_.bound(using ctx.extend(outs))(!dir)
  val clauses = subtypeDirSeq(oppositeBound, extrudedType, dir, outs)
  if checkSubtypeDir(bound, extrudedType, dir)(using ctx.extend(clauses)) then
    // Do not return a new bound if it is already satisfied in the context.
    clauses
  else
    val newBound = combine(dir.jointMode, bound, extrudedType)(using ctx.extend(clauses))
    SubClauses(Bound(var_, dir, newBound) :: clauses.elems)

// Rigid type variables.

def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      SubClauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      subtype(TVar(sub), sup.lowerBound)
    case (Order.Greater, _) =>
      subtype(sub.upperBound, TVar(sup))

/** Constrain a universal type to be a subtype of another type. */
def subtypeUnivSub(sub: TUniv, sup: Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val (univVars, univBody) = sub.getUnivComponents
  ctx.withSubtypingLevel((ctx) =>
    given SubContext = ctx
    val (instanceBody, cache, outs) = instantiateUniv(univVars, univBody, TypeVarKind.Flex)
    subtypeSeq(instanceBody, sup, outs)(using ctx.mapCache((_) => cache), mode)
  )

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val (univVars, univBody) = sup.getUnivComponents
  ctx.withSubtypingLevel((ctx) =>
    given SubContext = ctx
    val (instanceBody, cache, outs) = instantiateUniv(univVars, univBody, TypeVarKind.Rigid)
    subtypeSeq(sub, instanceBody, outs)(using ctx.mapCache((_) => cache), mode)
  )

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(constrained: TConstrained, type_ : Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val clauses = subtypeConstraint(constrained.constraint)
  subtypeSeq(constrained.body, type_, clauses)

/** Constrain a constrained type to be a supertype of another type. */
def subtypeConstrainedSup(constrained: TConstrained, type_ : Type)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val constraintClauses = try
    config.assumptionMode match
      case AssumptionMode.Flexify =>
        subtypeConstraint(constrained.constraint)(using ctx.flexify(), mode)
      case AssumptionMode.Reconstruct =>
        subtypeConstraint(constrained.constraint)(using ctx, ConstraintMode.Reconstruct)
  catch
    case error: TypeError =>
      if config.subtypeAbsurdConstreds then
        // Failure to reconstruct an assumption does not make it absurd when it can still refine
        // open inference variables. Only an assumption that also fails in solving mode is known
        // to make the constrained type vacuous.
        try
          subtypeConstraint(constrained.constraint)(using ctx, ConstraintMode.Solve)
        catch
          case _: TypeError =>
            return SubClauses.empty
      else
        throw error

  // TODO: While it makes sense to return new variables that may have been created in the constraints,
  // the only case where that happens currently results in infinite recursion.
  val bodyClauses = subtype(type_, constrained.body)(using ctx.extend(constraintClauses), mode)
  SubClauses(constraintClauses.typeVarDecls).concat(bodyClauses)

/** Constrain a tuple type to he a subtype of another tuple type. */
def subtypeTuple(sub: TTuple, sup: TTuple)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.all(
    subtype(sub.left,  sup.left),
    subtype(sub.right, sup.right),
  )

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.all(
    subtype(sup.param, sub.param),
    subtype(sub.ret,   sup.ret),
  )

/** Constrain a type application to be a subtype of another typa application. */
def subtypeApp(sub: TApp, sup: TApp)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.all(
    subtype(sub.abs, sup.abs),
    // Arguments are covariant for now.
    subtype(sub.arg, sup.arg),
  )

/** Constrain a set of bounds to be subsumed by another set of bounds. */
def subtypeBounds(subs: List[Bound], sups: List[Bound])(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  sups
    .foldRight(SubClauses.empty)((sup, clauses) =>
      val subTypes = subs.filterVarDir(sup.var_, sup.dir)
      val subType = subTypes.combineMany(sup.dir.jointMode)
      subtype(subType, sup.type_)
    )

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: SubContext): Boolean =
  try
    withCheckingMode(subtype(sub, sup)(using ctx.rigidify(), ConstraintMode.Solve))
  catch
    case _: TypeError =>
      return false

  return true

def checkSubtypeDir(sub: Type, sup: Type, dir: Direction)(using ctx: SubContext): Boolean =
  dir match
    case Direction.Sub =>
      checkSubtype(sub, sup)
    case Direction.Super =>
      checkSubtype(sup, sub)

/** Check whether tow types are equal without requiring any additional constraint. */
def checkEqual(left: Type, right: Type)(using ctx: SubContext): Boolean =
  val a =
    checkSubtype(left, right)
  val b =
    checkSubtype(right, left)
  a && b

/** Check if a bound is satisified in the context. */
def checkBound(bound: Bound)(using ctx: SubContext): Boolean =
  given SubContext = ctx
  checkSubtypeDir(TVar(bound.var_), bound.type_, bound.dir)

/** Check whether a subtyping constraint is satisfied in the context. */
def checkConstraint(constraint: Constraint)(using ctx: SubContext): Boolean =
  checkSubtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraint(constraint: Constraint)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  subtypeDir(constraint.left, constraint.right, constraint.dir)

def subtypeConstraintSeq(constraint: Constraint, ins: SubClauses)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  ctx.seqUnit(subtypeConstraint(constraint), ins)

/** Solve an unsolved constraint, raising an error if an error is found. */
def solve(clause: UnsolvedClause)(using ctx: SubContext): SubClauses =
  clause match
    case UnsolvedClause.Var(var_) =>
      SubClauses.single(TypeVarDecl(var_, TypeVarKind.Flex, None, ctx.level))
    case UnsolvedClause.Constr(constraint) =>
      subtypeConstraint(constraint)(using ctx, ConstraintMode.Solve)

/** Try to solve an unsolved constraint, returning `None` an error if an error is found. */
def trySolve(clause: UnsolvedClause)(using SubContext): Option[SubClauses] =
  try
    Some(solve(clause))
  catch
    case _: TypeError =>
      None

/** Solve an unsolved constraint, raising an error if an error is found. */
def solve(clauses: UnsolvedClauses)(using ctx: SubContext): SubClauses =
  clauses.elems.foldLeft(SubClauses.empty)((outs, clause) =>
    val outs2 = solve(clause)(using ctx.extend(outs))
    outs.concat(outs2)
  )

/** Try to solve some unsolved constraints, returning `None` an error if an error is found. */
def trySolve(clauses: UnsolvedClauses)(using ctx: SubContext): Option[SubClauses] =
  clauses.elems.foldLeft(Some(SubClauses.empty) : Option[SubClauses])((outs, clause) =>
    outs match
      case Some(outs) =>
        trySolve(clause)(using ctx.extend(outs)) match
          case Some(outs2) =>
            Some(outs.concat(outs2))
          case None =>
            None
      case None =>
        None
  )

/** Instantiate the quantified variables of a universal type at the given level, using fresh
 *  variables or approximations from the cache. */
def instantiateUniv(vars: List[TypeVar], body: Type, kind: TypeVarKind)(using ctx: SubContext): (Type, SubtypingCache, SubClauses) =
  var instanceBody = body
  var cache = ctx.cache
  var outs = SubClauses.empty
  for var_ <- vars do
    val decl = ctx.cache.checkUniv(var_, body) match
      case Some(instanceVar) =>
        instanceVar.decl(using ctx)
      case None =>
        val decl = ctx.declFreshVar(kind, var_)
        cache = cache.addUniv(var_, body, decl.var_)
        outs = outs.concat(decl.asSubClauses)
        decl

    instanceBody = instanceBody.substitute(var_, decl.var_)

  (instanceBody, cache, outs)
