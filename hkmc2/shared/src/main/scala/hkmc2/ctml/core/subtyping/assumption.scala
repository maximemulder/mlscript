package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.core.is
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

/** The judgments assumed on the current path of the subtyping search that are not bounds, which
 *  the hypothesis rule derives when they are met again exactly (see `assume` and `subtypeImpl`).
 *
 *  Like the trail, the hypotheses are passed down the search and never returned: they are scoped
 *  to the body of the constrained type whose constraint they are assumed from. */
case class Hypotheses(
  /** The judgments assumed, as pairs of a subtype and a supertype. */
  val judgments: Set[(Type, Type)] = Set(),
):
  /** Check whether a judgment is assumed. */
  def contains(sub: Type, sup: Type): Boolean =
    this.judgments.contains((sub, sup))

  /** Assume a judgment. */
  def add(sub: Type, sup: Type): Hypotheses =
    Hypotheses(this.judgments + ((sub, sup)))

extension (ctx: SubContext)
  /** Assume a constraint: get the context extended with the assumption, or `None` if the
   *  constraint is refuted (see `assume`). */
  def assumeConstraint(constraint: Constraint): Option[SubContext] =
    constraint.dir match
      case Direction.Sub =>
        ctx.assume(constraint.left, constraint.right)
      case Direction.Super =>
        ctx.assume(constraint.right, constraint.left)

  /** Assume a subtyping judgment: get the context extended with the assumption, or `None` if the
   *  judgment is refuted.
   *
   *  The judgment is decomposed by the invertible rules only, that is, the rules whose premises
   *  are equivalent to their conclusion, which thus apply to an assumption as they apply to a
   *  judgment to derive: the normalization of negations, the contraposition of negated variables,
   *  and the splits of `splitAll`. The judgments that no such rule decomposes are the leaves of the
   *  assumption:
   *  - A leaf that holds by itself (reflexivity, the extremal types, disjoint constructors) assumes
   *    nothing.
   *  - A leaf whose side is a variable is assumed as a bound of that variable, whether it is rigid
   *    or flexible, which the variable rules of `subtypeImpl` then read. A leaf between two
   *    variables is a bound of both.
 *  - Any other leaf is assumed as a hypothesis, which the hypothesis rule of `subtypeImpl`
 *    derives when it is met again exactly, and which nothing else uses.
   *
   *  The other rules would decompose an assumption into premises that it does not entail, since
   *  their conclusion holds in cases where their premises do not: the choice rules (`S1 ∧ S2 ≤ Int`
   *  holds for `S1 = Str` and `S2 = Bool` while neither `S1 ≤ Int` nor `S2 ≤ Int` does), the
   *  lambda and tuple rules (`⊤ → S1 ≤ ⊥ → Int` holds whatever `S1` is, since `⊥ → Int` is the
   *  type of all functions), the binder rules (`({S1 ≤ Int} ⟹ Bool) ≤ ⊤` holds whatever `S1` is),
   *  and the replacement of a variable by its bound (an upper bound `τ` of `α` only approximates
   *  `α`, so that `τ ≤ Int` does not follow from `α ≤ Int`). This is the saturation of the guards
   *  in the mechanization, whose leaves that are not bounds are the residues.
   *
   *  Under the `ex-falso` option, a leaf that is refuted makes the assumption absurd, so that the
   *  judgment is not assumed: a constrained type whose constraint is absurd is `⊤`, so it is a
   *  supertype of every type. A leaf is refuted when it does not hold between decided types (see
   *  `isDecided`), on which the subtyping rules are complete: a bound `α ≤ τ` is refuted through
   *  the lower bound of `α`, which `α` is at least, and dually. This is valid in the model of
   *  types, in which every class is inhabited, but not derivable, like the complement rules (see
   *  `admitsComplement`). It is not valid on other types, on which a judgment that is not derived
   *  may hold: `Int → Int ≤ ⊥ → Str` holds in the model. */
  def assume(sub: Type, sup: Type): Option[SubContext] =
    given SubContext = ctx

    // Leaves that hold by themselves.

    if isReflexive(sub, sup) || sub.is[TBot] || sup.is[TTop] then
      return Some(ctx)

    // Normalization of negations.

    sub match
      case TNeg(sub) =>
        sub.negateStep() match
          case Some(sub) =>
            return ctx.assume(sub, sup)
          case _ =>
      case _ =>

    sup match
      case TNeg(sup) =>
        sup.negateStep() match
          case Some(sup) =>
            return ctx.assume(sub, sup)
          case _ =>
      case _ =>

    (sub, sup) match
      case (TNeg(sub), TNeg(sup)) =>
        return ctx.assume(sup, sub)
      case _ =>

    sup match
      case TNeg(sup) if areDisjointConstructors(sub, sup) =>
        return Some(ctx)
      case _ =>

    // Variables, including negated variables, which are contraposed to the other side: `¬α ≤ τ`
    // iff `¬τ ≤ α`, and `τ ≤ ¬α` iff `α ≤ ¬τ`.

    (sub, sup) match
      case (TVar(sub), TVar(sup)) if sub == sup =>
        return Some(ctx)
      case (TVar(sub), TVar(sup)) =>
        return ctx.assumeBound(sub, Direction.Sub, TVar(sup)).flatMap(_.assumeBound(sup, Direction.Super, TVar(sub)))
      case (TVar(sub), _) =>
        return ctx.assumeBound(sub, Direction.Sub, sup)
      case (_, TVar(sup)) =>
        return ctx.assumeBound(sup, Direction.Super, sub)
      case (TNeg(TVar(sub)), _) =>
        return ctx.assume(sup.negate(), TVar(sub))
      case (_, TNeg(TVar(sup))) =>
        return ctx.assume(TVar(sup), sub.negate())
      case _ =>

    // Invertible splits, which never replace a variable by its bound.

    sub.splitAll(Polarity.Negative, sup, false) match
      case Some(subLeft, subRight) =>
        return ctx.assume(subLeft, sup).flatMap(_.assume(subRight, sup))
      case _ =>

    sup.splitAll(Polarity.Positive, sub, false) match
      case Some(supLeft, supRight) =>
        return ctx.assume(sub, supLeft).flatMap(_.assume(sub, supRight))
      case _ =>

    // Hypotheses.

    if config.exFalso && isRefuted(sub, sup) then
      return None

    Some(ctx.mapHypotheses(_.add(sub, sup)))

  /** Assume a bound of a type variable, or get `None` if the bound is refuted (see `assume`). */
  private def assumeBound(var_ : TypeVar, dir: Direction, type_ : Type): Option[SubContext] =
    given SubContext = ctx
    if config.exFalso && isRefutedDir(var_.bound(!dir), type_, dir) then
      return None

    Some(ctx.extend(Bound(var_, dir, type_, BoundKind.Asserted)))

/** Check whether a subtyping judgment is refuted, that is, whether it is between decided types and
 *  does not hold (see `assume`). */
private def isRefuted(sub: Type, sup: Type)(using ctx: SubContext): Boolean =
  sub.isDecided && sup.isDecided && !checkSubtype(sub, sup)

/** Check whether a subtyping judgment in a given typing direction is refuted (see `isRefuted`). */
private def isRefutedDir(left: Type, right: Type, dir: Direction)(using ctx: SubContext): Boolean =
  dir match
    case Direction.Sub =>
      isRefuted(left, right)
    case Direction.Super =>
      isRefuted(right, left)
