package hkmc2.ctml.core.subtyping

import scala.collection.immutable.Set as Set

import hkmc2.ctml.config.*
import hkmc2.ctml.core.{is, isSubClass, makeJointType}
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
  // Effective bounds are combinations of asserted bounds, which are constrained themselves.
  clauses.removeEffectiveBounds().elems.foldRight(SubClauses.empty)((clause, clauses) => ctx.seqUnit(constrainClause(clause), clauses))

/** Constrain a clause to hold in the context. */
def constrainClause(clause: SubClause)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  clause match
    case decl: ClassDecl =>
      SubClauses.single(decl)
    case decl: TypeVarDecl =>
      SubClauses.single(decl)
    case Bound(var_, dir, type_, _) =>
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

  // Handle the reflexion case, up to the choice rules (see `isReflexive`).

  if isReflexive(sub, sup) then
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

  // A negation on one side only is moved to the other side by contraposition: `σ ≤ ¬τ` iff
  // `τ ≤ ¬σ`, and `¬τ ≤ σ` iff `¬σ ≤ τ`, both derivable from the negation and double negation
  // rules. Contraposition is applied when the negated type is a binder type (a universal or
  // constrained type), which the binder rules below only decompose when it is not negated: after
  // contraposition, the binder is decomposed and its negation disappears.

  // Contraposition is not applied to any negation, as it alone would loop between `σ ≤ ¬τ` and
  // `τ ≤ ¬σ`. For the same reason, if the other side is itself a binder type, it is decomposed
  // first rather than turned into a negated binder type by contraposition.

  // Notably, these rules derive the rules of constraining types, which are encoded as negated
  // constrained types `¬({c} ⟹ ¬τ)` (see `makeConstrainingType`):
  // - `σ ≤ ¬({c} ⟹ ¬τ)` iff `({c} ⟹ ¬τ) ≤ ¬σ`, that is, `c` is solved and `σ ≤ τ`.
  // - `¬({c} ⟹ ¬τ) ≤ σ` iff `¬σ ≤ ({c} ⟹ ¬τ)`, that is, `τ ≤ σ` assuming `c`.

  // These rules come before the flexible variable rules, where the rules of the former primitive
  // constraining types were, so that the constraining type in a flexible variable bound is
  // decomposed rather than kept whole. Placing them after the flexible variable rules, with the
  // constrained type rules, was tried: it made inferred types larger, and did not avoid the
  // time-outs of `ctmlFlowConstraningWeirdMatch.mls`.

  sub match
    case TNeg(subBody) if subBody.isBinder =>
      return sup match
        case sup: TUniv =>
          subtypeUnivSup(sub, sup)
        case sup: TConstrained =>
          subtypeConstrainedSup(sup, sub)
        case _ =>
          subtype(sup.negate(), subBody)
    case _ =>

  sup match
    case TNeg(supBody) if supBody.isBinder =>
      return sub match
        case sub: TUniv =>
          subtypeUnivSub(sub, sup)
        case sub: TConstrained =>
          subtypeConstrainedSub(sub, sup)
        case _ =>
          subtype(supBody, sub.negate())
    case _ =>

  // A negated flexible variable on one side only is also moved to the other side by
  // contraposition, so that the variable is bounded by the flexible variable rules below:
  // `¬α ≤ τ` iff `¬τ ≤ α`, and `τ ≤ ¬α` iff `α ≤ ¬τ`.

  // This rule does not apply when the other side is a binder type, which the binder rules below
  // decompose first, since contraposition would turn it into a negated binder type, which the
  // rules above would then contrapose back. It does not apply either when the other side is a
  // flexible variable, which the flexible variable rules below bound by the negated variable,
  // since contraposition would turn it into a negated flexible variable, which this rule would
  // then contrapose back.

  (sub, sup) match
    case (TNeg(TVar(subVar)), _) if subVar.isFlexMode && !sup.isBinder && !sup.isFlexModeVar =>
      return subtype(sup.negate(), TVar(subVar))
    case (_, TNeg(TVar(supVar))) if supVar.isFlexMode && !sub.isBinder && !sub.isFlexModeVar =>
      return subtype(TVar(supVar), sub.negate())
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

  // Assume the constraint of a right constrained type before decomposing the subtype as well:
  // `τ ≤ {c} ⟹ σ` iff `τ ≤ σ` assuming `c`, which loses no derivation, so that the constraint is
  // assumed once for every branch of the same judgment.

  // This rule used to come after the choice rules and the rules of rigid variables, with the rule of
  // left constrained types. An operand of the subtype was then chosen, or a rigid subtype replaced
  // by its upper bound, before the constraint was assumed, although neither of these rules is
  // invertible, and both depend on the assumed bounds: e.g. `(A → C) ∧ (B → C) ≤ {α ≤ A ∨ B} ⟹ α → C`
  // was explored as `A → C ≤ {α ≤ A ∨ B} ⟹ α → C` or `B → C ≤ {α ≤ A ∨ B} ⟹ α → C`, which both
  // fail, and `α ≤ {α ≤ Int} ⟹ Int` as `⊤ ≤ {α ≤ Int} ⟹ Int`. This is also the order of the paper,
  // in which `C-ConstredR` is attempted with `C-ForallR`, before `C-VarBound` and `C-JointAny`.

  // The previous order did let an operand of the subtype that is equal to the constrained type be
  // chosen before the constraint is assumed, and so be compared to it by reflexivity. This is now
  // done by the reflexion case (see `isReflexive`).

  sup match
    case sup: TConstrained =>
      return subtypeConstrainedSup(sup, sub)
    case _ =>

  // Subtyping of union and intersection types.

  // The invertible rules are applied first, on both sides: a left union and a right intersection
  // are each decomposed in two judgments that must both hold, which loses no derivation (see
  // `splitAll`).

  sub.splitAll(Polarity.Negative, sup) match
    case Some(subLeft, subRight) =>
      return ctx.all(
        subtype(subLeft,  sup),
        subtype(subRight, sup),
      )
    case _ =>

  // Two lambda types are compared as they are, before the supertype is decomposed. Decomposing it
  // is only useful to a subtype that may use a different operand or instance for each of its parts,
  // which a single lambda type cannot: `τ → σ ≤ (τ₁ ∨ τ₂) → ρ` iff `τ₁ ∨ τ₂ ≤ τ` and `σ ≤ ρ`.

  // This rule used to come last, with the rules of the other type constructors. The supertype was
  // then decomposed first, which compared the other components of the lambda types once per part,
  // and replaced the rigid variables that it was decomposed through by their bound: e.g. given an
  // upper bound `A ∨ B` of `α`, `β → C ≤ α → C` required `A ∨ B ≤ β` rather than `α ≤ β` from a
  // flexible variable `β`.

  (sub, sup) match
    case (sub: TLam, sup: TLam) =>
      return subtypeLam(sub, sup)
    case _ =>

  sup.splitAll(Polarity.Positive, sub) match
    case Some(supLeft, supRight) =>
      return ctx.all(
        subtype(sub, supLeft),
        subtype(sub, supRight),
      )
    case _ =>

  // The choice rules are applied last, on what the invertible rules cannot decompose: a right union
  // and a left intersection are each decomposed in two judgments of which either may hold, and which
  // forget the other operand (see `splitAny`).

  // Right unions used to be split before right intersections, which distributed them over the
  // intersection: `τ ≤ (σ₁ ∨ σ₂) ∧ σ₃` was explored as `τ ≤ σ₁ ∧ σ₃` or `τ ≤ σ₂ ∧ σ₃`. This was
  // exponential in the number of unions of the intersection, which notably made checks against
  // intersections of unions of constraining types time out (e.g. `foo(1, 1) as Int` in
  // `ctmlFlowWeirdMatch.mls`). It also prevented `joinMerge` from merging the unions, so that
  // `⊤ ≤ (A ∨ ¬A) ∧ (B ∨ ¬B)` failed.

  sup.splitAny(Polarity.Positive) match
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

  sub.splitAny(Polarity.Negative) match
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

  // Subtyping of left constrained types.

  // The right constrained types are decomposed first (see above), so that the left constraint
  // (which must be solvable) may be solved using the assumptions of the right constraint (which may
  // not be solvable).

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
      subtypeSeq(sup.lowerBound, sub.upperBound, y.concat(makeBoundClauses(sup, Direction.Super, TVar(sub), supLowerBound)))
    case (Order.Greater, _) =>
      val x = subtype(sub.lowerBound, TVar(sup))
      val subUpperBound = meet(TVar(sup), sub.upperBound)
      subtypeSeq(sup.lowerBound, sub.upperBound, x.concat(makeBoundClauses(sub, Direction.Sub, TVar(sup), subUpperBound)))

/** Constrain a type variable to be subtype or supertype of another type. */
def subtypeFlexVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: SubContext, mode: ConstraintMode): SubClauses =
  val (extrudedType, outs) = if config.extrudeVar then
    type_.extrude(var_.level, dir.rightPol)
  else
    (type_, SubClauses.empty)

  val bound = var_.bound(using ctx.extend(outs))(dir)
  val oppositeBound = var_.bound(using ctx.extend(outs))(!dir)
  val clauses = subtypeDirSeq(oppositeBound, extrudedType, dir, outs)
  // The new type is asserted as a bound of the variable, and its combination with the current bound
  // becomes the effective bound of the variable, so that it does not need to be recomputed whenever
  // the variable is used (see `BoundKind`).
  val effectiveBound = mode match
    // In solving mode, the new bound is part of the solution, so it is kept as simple as possible:
    // it is not asserted if it is already satisfied in the context, and the effective bound is
    // otherwise simplified using subtyping checks.
    case ConstraintMode.Solve =>
      if checkSubtypeDir(bound, extrudedType, dir)(using ctx.extend(clauses)) then
        return clauses
      combine(dir.jointMode, bound, extrudedType)(using ctx.extend(clauses))
    // In reconstruction mode, the new bound is an assumption of a constrained type, which is
    // discarded once the body of the constrained type has been constrained (see
    // `subtypeConstrainedSup`), so the effective bound is only simplified syntactically. Simplifying
    // it as in solving mode requires subtyping checks, which reconstruct the assumptions of the
    // constrained types they meet, and so on: since upper bounds are joined using constraining
    // types, whose guards contain the other bounds of their branch, these nested checks made up most
    // of the type checking time of matches (e.g. it took minutes to infer the type of the
    // two-parameter match function of `ctmlFlow.mls`, and now takes a fraction of a second).
    case ConstraintMode.Reconstruct =>
      val effectiveBound = makeJointType(dir.jointMode, extrudedType, bound)
      if effectiveBound == bound then
        return clauses
      effectiveBound

  clauses.concat(makeBoundClauses(var_, dir, extrudedType, effectiveBound))

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
  // The effective bounds of the body may rely on the assumed constraint, so they are removed when
  // leaving its scope, unlike the asserted bounds of the body (see `BoundKind`).
  SubClauses(constraintClauses.typeVarDecls).concat(bodyClauses.removeEffectiveBounds())

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
