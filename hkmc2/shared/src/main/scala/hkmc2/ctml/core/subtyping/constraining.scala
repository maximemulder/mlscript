package hkmc2.ctml.core.subtyping

import scala.collection.immutable.Set as Set

import hkmc2.ctml.config.*
import hkmc2.ctml.core.{is, isSubClass}
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
def constrainClauses(clauses: SubClauses)(using ctx: SubContext): SubClauses =
  // Effective bounds are combinations of asserted bounds, which are constrained themselves.
  clauses.removeEffectiveBounds().elems.foldRight(SubClauses.empty)((clause, clauses) => ctx.seqUnit(constrainClause(clause), clauses))

/** Constrain a clause to hold in the context. */
def constrainClause(clause: SubClause)(using ctx: SubContext): SubClauses =
  clause match
    case decl: ClassDecl =>
      SubClauses.single(decl)
    case decl: TypeVarDecl =>
      SubClauses.single(decl)
    case Bound(var_, dir, type_, _) =>
      subtypeDir(TVar(var_), type_, dir)

/** Sequentially constrain a type to be a subtype of another type in a context. */
def subtypeSeq(sub: Type, sup: Type, ins: SubClauses)(using ctx: SubContext): SubClauses =
  ctx.seqUnit(subtype(sub, sup), ins)

/** Constrain a type to be a subtype or supertype of another type according to a typing direction. */
def subtypeDir(left: Type, right: Type, dir: Direction)(using ctx: SubContext): SubClauses =
  dir match
    case Direction.Sub =>
      subtype(left, right)
    case Direction.Super =>
      subtype(right, left)

/** Sequentially constrain a type to be a subtype or supertype of another type according to a typing direction in a context. */
def subtypeDirSeq(left: Type, right: Type, dir: Direction, ins: SubClauses)(using ctx: SubContext): SubClauses =
  ctx.seqUnit(subtypeDir(left, right, dir), ins)

/** Constrain a type to be a subtype of another type in a context. */
def subtype(sub: Type, sup: Type)(using ctx: SubContext): SubClauses =
  try
    subtypeWithDebug(subtypeTrail)(sub, sup)
  catch
    case error: TypeError =>
      error.addStep(SubtypingJudgment(sub, sup))
      throw error

/** Implementation of `subtype` with the trail of the judgments in progress (see `SubtypingTrail`).
 *
 *  A judgment that repeats a judgment in progress is answered according to the rule that resolves
 *  the judgment in progress:
 *  - A judgment whose side is a flexible variable, or a negated flexible variable, is a bound of
 *    that variable, which is checked against the opposite bounds of the variable before being
 *    recorded (see `subtypeFlexVar`). The repeated judgment is satisfied by that bound, as by the
 *    hypothesis rule of the paper (`C-Hyp`), so it is discharged.
 *  - Otherwise, the judgment in progress is resolved through the bounds of rigid variables, and
 *    repeats because these bounds form a cycle. Discharging it would assume the judgment in order
 *    to derive it, which is unsound: `A ≤ Str` does not follow from `A ≤ B ∨ Str` and
 *    `B ≤ A ∨ Str`, which `A = B = Int` satisfies. No derivation goes through the cycle, so the
 *    repeated judgment fails, and the search goes on to its next alternative, as the Lean
 *    mechanization does on the exact repeats of its trail. */
def subtypeTrail(sub: Type, sup: Type)(using ctx: SubContext): SubClauses =
  if ctx.trail.contains(sub, sup) then
    val discharged = sub.lookupVar.exists(_.isFlex) || sup.lookupVar.exists(_.isFlex)
    debugTrail(sub, sup, discharged)
    if !discharged then
      throw TypeError(Some(
        s"Judgment ${sub} ≤ ${sup} repeats a judgment in progress through a cycle of bounds."
      ))
    return SubClauses.empty

  subtypeImpl(sub, sup)(using ctx.mapTrail(_.add(sub, sup)))

/** Implementation of `subtype` by the subtyping rules. */
def subtypeImpl(sub: Type, sup: Type)(using ctx: SubContext): SubClauses =

  // Handle the reflexion case, up to the choice rules (see `isReflexive`).

  if isReflexive(sub, sup) then
    return SubClauses.empty

  // A judgment that is assumed holds, by the hypothesis rule (see `assume`).

  if config.hypothesis && ctx.hypotheses.contains(sub, sup) then
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

  // These rules come before the flexible variable rules, so that the constraining type in a
  // flexible variable bound is decomposed rather than kept whole. Placing them after the flexible
  // variable rules, with the constrained type rules, makes inferred types larger.

  // A constrained type that is moved to the left by contraposition is decomposed right away, which
  // is the introduction rule of constraining types above. Deferring it to the rule of left
  // constrained types would let the choice rules decompose the negated subtype first: e.g.
  // `Int ∧ ¬Nat ≤ ¬({c} ⟹ ¬(Int ∧ ¬Nat))` would be explored as `{c} ⟹ ¬(Int ∧ ¬Nat) ≤ ¬Int` or
  // `{c} ⟹ ¬(Int ∧ ¬Nat) ≤ Nat`, which both fail. A universal type that is moved to the left is
  // still decomposed after the invertible rules, so that it is instantiated once per operand of a
  // right intersection.

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
          supBody match
            case supBody: TConstrained =>
              subtypeConstrainedSub(supBody, sub.negate())
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
    case (TNeg(TVar(subVar)), _) if subVar.isFlex && !sup.isBinder && !sup.isFlexVar =>
      return subtype(sup.negate(), TVar(subVar))
    case (_, TNeg(TVar(supVar))) if supVar.isFlex && !sub.isBinder && !sub.isFlexVar =>
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

  // Subtyping of flexible type variables.

  (sub, sup) match
    case (TVar(sub), TVar(sup)) if sub.isFlex && sup.isFlex =>
      return subtypeFlexVars(sub, sup)
    case (TVar(sub), _) if sub.isFlex =>
      return subtypeFlexVar(sub, sup, Direction.Sub)
    case (_, TVar(sup)) if sup.isFlex =>
      return subtypeFlexVar(sup, sub, Direction.Super)
    case (_, _) =>

  // Expand transparent rigid aliases before structural decomposition, so that rules under the
  // alias (notably right universal introduction) govern the whole judgment.

  // TODO: Investigate this block.

  (sub, sup) match
    case (TVar(sub), _) if sub.isRigid && sub.lowerBound == sub.upperBound =>
      return subtype(sub.upperBound, sup)
    case (_, TVar(sup)) if sup.isRigid && sup.lowerBound == sup.upperBound =>
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

  // This rule comes before the choice rules and the rules of rigid variables, which are not
  // invertible, and depend on the assumed bounds: choosing an operand of the subtype first explores
  // `(A → C) ∧ (B → C) ≤ {α ≤ A ∨ B} ⟹ α → C` as `A → C ≤ {α ≤ A ∨ B} ⟹ α → C` or
  // `B → C ≤ {α ≤ A ∨ B} ⟹ α → C`, which both fail, and replacing a rigid subtype by its upper
  // bound first explores `α ≤ {α ≤ Int} ⟹ Int` as `⊤ ≤ {α ≤ Int} ⟹ Int`. This is also the order of
  // the paper, in which `C-ConstredR` is attempted with `C-ForallR`, before `C-VarBound` and
  // `C-JointAny`.

  // An operand of the subtype that is equal to the constrained type is thus not compared to it after
  // a choice rule, but by the reflexivity check that precedes all the rules (see `isReflexive`).

  sup match
    case sup: TConstrained =>
      return subtypeConstrainedSup(sup, sub)
    case _ =>

  // Subtyping of union and intersection types.

  // The invertible rules are applied first, on both sides: a left union and a right intersection
  // are each decomposed in two judgments that must both hold, which loses no derivation (see
  // `splitAll`).

  sub.splitAll(Polarity.Negative, sup, true) match
    case Some(subLeft, subRight) =>
      return ctx.all(
        subtype(subLeft,  sup),
        subtype(subRight, sup),
      )
    case _ =>

  // Two lambda types are compared as they are, before the supertype is decomposed. Decomposing it
  // is only useful to a subtype that may use a different operand or instance for each of its parts,
  // which a single lambda type cannot: `τ → σ ≤ (τ₁ ∨ τ₂) → ρ` iff `τ₁ ∨ τ₂ ≤ τ` and `σ ≤ ρ`.

  // Decomposing the supertype first would compare the other components of the lambda types once
  // per part, and replace the rigid variables that it is decomposed through by their bound: e.g.
  // given an upper bound `A ∨ B` of `α`, `β → C ≤ α → C` would require `A ∨ B ≤ β` rather than
  // `α ≤ β` from a flexible variable `β`.

  (sub, sup) match
    case (sub: TLam, sup: TLam) =>
      return subtypeLam(sub, sup)
    case _ =>

  sup.splitAll(Polarity.Positive, sub, true) match
    case Some(supLeft, supRight) =>
      return ctx.all(
        subtype(sub, supLeft),
        subtype(sub, supRight),
      )
    case _ =>

  // The choice rules are applied last, on what the invertible rules cannot decompose: a right union
  // and a left intersection are each decomposed in two judgments of which either may hold, and which
  // forget the other operand (see `splitAny`).

  // Right intersections are thus split before right unions. Splitting right unions first would
  // distribute them over the intersection: `τ ≤ (σ₁ ∨ σ₂) ∧ σ₃` would be explored as `τ ≤ σ₁ ∧ σ₃`
  // or `τ ≤ σ₂ ∧ σ₃`. This is exponential in the number of unions of the intersection, which
  // notably makes checks against intersections of unions of constraining types time out (e.g.
  // `foo(1, 1) as Int` in `ctmlFlowWeirdMatch.mls`). It also prevents `joinMerge` from merging the
  // unions, so that `⊤ ≤ (A ∨ ¬A) ∧ (B ∨ ¬B)` fails.

  // A subtype that is split between the operands of a right union is derived by cases on a class of
  // the union, only once the choice of a single operand fails (see `subtypeCases`).

  sup.splitAny(Polarity.Positive) match
    case Some(supLeft, supRight) =>
      return joinMerge(supLeft, supRight) match
        case Some(sup) =>
          subtype(sub, sup)
        case None =>
          try
            ctx.any(
              subtype(sub, supLeft),
              subtype(sub, supRight),
            )
          catch
            case error: TypeError =>
              subtypeCases(sub, sup, error)
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
    case (TVar(sub), TVar(sup)) if sub.isRigid && sup.isRigid =>
      return subtypeRigidVars(sub, sup)
    case (TVar(sub), _) if sub.isRigid =>
      return subtype(sub.upperBound, sup)
    case (_, TVar(sup)) if sup.isRigid =>
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

  throw TypeError()

// Flexible type variables.

/** Constrain a type variable to be subtype of another type variable. */
def subtypeFlexVars(sub: TypeVar, sup: TypeVar)(using ctx: SubContext): SubClauses =
  // The constraint `α ≤ β` is recorded as a bound of the variable of the higher level, so that the
  // bound does not mention a variable above the level of its own variable. It is checked by
  // constraining the other variable against the opposite bound of this variable: `α ≤ U_β` for a
  // new lower bound `α` of `β`, and `L_α ≤ β` for a new upper bound `β` of `α`. Both check
  // `L_α ≤ U_β` and propagate the bound to the other variable (see `subtypeFlexVar`). No other
  // constraint follows from `α ≤ β`, which notably does not relate `L_β` and `U_α`.
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      SubClauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      val clauses = subtype(TVar(sub), sup.upperBound)
      val supLowerBound = join(TVar(sub), sup.lowerBound)
      clauses.concat(makeBoundClauses(sup, Direction.Super, TVar(sub), supLowerBound))
    case (Order.Greater, _) =>
      val clauses = subtype(sub.lowerBound, TVar(sup))
      val subUpperBound = meet(TVar(sup), sub.upperBound)
      clauses.concat(makeBoundClauses(sub, Direction.Sub, TVar(sup), subUpperBound))

/** Constrain a type variable to be subtype or supertype of another type. */
def subtypeFlexVar(var_ : TypeVar, type_ : Type, dir: Direction)(using ctx: SubContext): SubClauses =
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
  // The new bound is part of the solution, so it is kept as simple as possible: it is not asserted
  // if it is already satisfied in the context, and the effective bound is otherwise simplified
  // using subtyping checks.
  if checkSubtypeDir(bound, extrudedType, dir)(using ctx.extend(clauses)) then
    return clauses
  val effectiveBound = combine(dir.jointMode, bound, extrudedType)(using ctx.extend(clauses))

  clauses.concat(makeBoundClauses(var_, dir, extrudedType, effectiveBound))

// Rigid type variables.

/** Constrain a rigid type variable to be a subtype of another rigid type variable. */
def subtypeRigidVars(sub: TypeVar, sup: TypeVar)(using ctx: SubContext): SubClauses =
  (Order.compare(sub.level, sup.level), ctx.compareVarLevels(sub, sup)) match
    // If both variables are equal then they are subtype.
    case (Order.Equal, Order.Equal) =>
      SubClauses.empty
    case (Order.Lesser | Order.Equal, _) =>
      subtype(TVar(sub), sup.lowerBound)
    case (Order.Greater, _) =>
      subtype(sub.upperBound, TVar(sup))

/** Constrain a universal type to be a subtype of another type. */
def subtypeUnivSub(sub: TUniv, sup: Type)(using ctx: SubContext): SubClauses =
  val (univVars, univBody) = sub.getUnivComponents
  ctx.withSubtypingLevel((ctx) =>
    val (instanceBody, outs) = instantiateUniv(univVars, univBody, TypeVarKind.Flex)(using ctx)
    subtypeSeq(instanceBody, sup, outs)(using ctx)
  )

/** Constrain a universal type to be a supertype of another type.. */
def subtypeUnivSup(sub: Type, sup: TUniv)(using ctx: SubContext): SubClauses =
  val (univVars, univBody) = sup.getUnivComponents
  ctx.withSubtypingLevel((ctx) =>
    val (instanceBody, outs) = instantiateUniv(univVars, univBody, TypeVarKind.Rigid)(using ctx)
    subtypeSeq(sub, instanceBody, outs)(using ctx)
  )

/** Constrain a constrained type to be a subtype of another type. */
def subtypeConstrainedSub(constrained: TConstrained, type_ : Type)(using ctx: SubContext): SubClauses =
  val clauses = subtypeConstraint(constrained.constraint)
  subtypeSeq(constrained.body, type_, clauses)

/** Constrain a constrained type to be a supertype of another type: `τ ≤ {c} ⟹ σ` iff `τ ≤ σ`
 *  assuming `c` (see `assume`). */
def subtypeConstrainedSup(constrained: TConstrained, type_ : Type)(using ctx: SubContext): SubClauses =
  ctx.assumeConstraint(constrained.constraint) match
    case Some(ctx) =>
      // The effective bounds of the body may rely on the assumed constraint, so they are removed
      // when leaving its scope, unlike the asserted bounds of the body (see `BoundKind`).
      subtype(type_, constrained.body)(using ctx).removeEffectiveBounds()
    case None =>
      // The constraint is refuted, so that the constrained type is `⊤`.
      SubClauses.empty

/** Constrain a tuple type to he a subtype of another tuple type. */
def subtypeTuple(sub: TTuple, sup: TTuple)(using ctx: SubContext): SubClauses =
  ctx.all(
    subtype(sub.left,  sup.left),
    subtype(sub.right, sup.right),
  )

/** Constrain a lambda type to be a subtype of another lambda type. */
def subtypeLam(sub: TLam, sup: TLam)(using ctx: SubContext): SubClauses =
  ctx.all(
    subtype(sup.param, sub.param),
    subtype(sub.ret,   sup.ret),
  )

/** Constrain a type application to be a subtype of another typa application. */
def subtypeApp(sub: TApp, sup: TApp)(using ctx: SubContext): SubClauses =
  ctx.all(
    subtype(sub.abs, sup.abs),
    // Arguments are covariant for now.
    subtype(sub.arg, sup.arg),
  )

/** Check whether a type is a subtype of another type without requiring any additional constraint. */
def checkSubtype(sub: Type, sup: Type)(using ctx: SubContext): Boolean =
  try
    withCheckingMode(subtype(sub, sup)(using ctx.rigidify()))
  catch
    case _: TypeError =>
      return false

  return true

/** Check whether a type is a subtype or supertype of another type according to a typing direction
 *  without requiring any additional constraint. */
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

/** Constrain a subtyping constraint to hold in a context. */
def subtypeConstraint(constraint: Constraint)(using ctx: SubContext): SubClauses =
  subtypeDir(constraint.left, constraint.right, constraint.dir)

/** Sequentially constrain a subtyping constraint to hold in a context. */
def subtypeConstraintSeq(constraint: Constraint, ins: SubClauses)(using ctx: SubContext): SubClauses =
  ctx.seqUnit(subtypeConstraint(constraint), ins)

/** Solve an unsolved constraint, raising an error if an error is found. */
def solve(clause: UnsolvedClause)(using ctx: SubContext): SubClauses =
  clause match
    case UnsolvedClause.Var(var_) =>
      SubClauses.single(TypeVarDecl(var_, TypeVarKind.Flex, None, ctx.level))
    case UnsolvedClause.Constr(constraint) =>
      subtypeConstraint(constraint)(using ctx)

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

/** Instantiate the quantified variables of a universal type with fresh variables of a given kind
 *  at the current level, and return the instance and the declarations of the fresh variables. */
def instantiateUniv(vars: List[TypeVar], body: Type, kind: TypeVarKind)(using ctx: SubContext): (Type, SubClauses) =
  val decls = ctx.declFreshVars(vars, kind)
  val instanceBody = vars.zip(decls).foldLeft(body)((body, pair) =>
    body.substitute(pair(0), pair(1).var_)
  )
  (instanceBody, SubClauses(decls.reverse))
