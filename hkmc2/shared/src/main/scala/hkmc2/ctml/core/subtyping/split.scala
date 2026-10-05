package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

// Decomposition of the union and intersection types of a subtyping judgment.

// The joint types of a judgment `τ ≤ σ` are decomposed by two kinds of rules, which are selected by
// the polarity of the type alone: `τ` is at the negative polarity, and `σ` at the positive one.
// - The invertible rules decompose a union at the negative polarity (`τ₁ ∨ τ₂ ≤ σ` iff `τ₁ ≤ σ` and
//   `τ₂ ≤ σ`) and an intersection at the positive polarity (`τ ≤ σ₁ ∧ σ₂` iff `τ ≤ σ₁` and
//   `τ ≤ σ₂`). Both judgments must hold, and no derivation is lost by applying them (see
//   `splitAll`).
// - The choice rules decompose an intersection at the negative polarity (`τ₁ ∧ τ₂ ≤ σ` if `τ₁ ≤ σ`
//   or `τ₂ ≤ σ`) and a union at the positive polarity (`τ ≤ σ₁ ∨ σ₂` if `τ ≤ σ₁` or `τ ≤ σ₂`).
//   Either judgment may hold, and each of them forgets an operand, so that these rules are not
//   invertible (see `splitAny`).

// These two operations replace a single `split` function, which took the mode of the joint type to
// expose independently of the polarity, and so accepted combinations that are not rules:
// - It distributed intersections over unions to expose a union at the positive polarity, which was
//   explored by the choice rule, in a time exponential in the number of unions. This used to be
//   worked around by a separate split of outer intersections (`splitOuterInter`), applied before
//   the choice rule.
// - It split lambda types into intersections at the negative polarity, where the choice rule then
//   kept a single one of them: `(A ∨ B) → C ≤ α → C` was not derived from `α ≤ A ∨ B`, since it was
//   explored as `A → C ≤ α → C` or `B → C ≤ α → C`.
// - It kept the same polarity under negations and in lambda parameters, and so replaced the
//   variables that occur there by their bound of the wrong direction, which is unsound: e.g.
//   `C ≤ ¬α` was derived from a lower bound `(A → A) ∧ (B → B)` of `α`, and `α → C ≤ A → C` from an
//   upper bound `A ∨ B` of `α`.

/** The kind of rule that decomposes a joint type in a subtyping judgment. */
private enum SplitRule:
  /** The invertible rules, whose two judgments must both hold. */
  case All
  /** The choice rules, of which either of the two judgments may hold. */
  case Any

  /** Check whether the rule decomposes the joint types of a given mode at a given polarity. */
  def decomposes(mode: JointMode, pol: Polarity): Boolean =
    this match
      case All => !mode.isNaturalPol(pol)
      case Any => mode.isNaturalPol(pol)

/** Check whether a judgment holds by reflexivity alone, up to the choice rules, that is, whether an
 *  operand of the nested intersections of the subtype is also an operand of the nested unions of
 *  the supertype: e.g. `τ ∧ σ ≤ τ ∨ ρ`.
 *
 *  This is checked before any other rule, since the other rules may turn such a judgment into
 *  judgments that are not derived. Only the two sides as a whole used to be compared, which relied
 *  on the choice rules to reach their operands, and so on the rules that are applied before the
 *  choice rules to keep these operands as they are: e.g. `({c} ⟹ α) ∧ τ ≤ {c} ⟹ α` held by choosing
 *  the left operand of the subtype, which no longer happens before the right constrained type is
 *  opened (see `subtypeImpl`). It then requires `{c} ⟹ α ≤ α`, which is not derived when `α` is
 *  rigid: `α` is replaced by its lower bound before the left constrained type is opened. */
def isReflexive(sub: Type, sup: Type): Boolean =
  sup.hasJointOperand(JointMode.Union, sub) || (sub match
    case TJointType(JointMode.Inter, left, right) =>
      isReflexive(left, sup) || isReflexive(right, sup)
    case _ =>
      false
  )

extension (type_ : Type)
  /** Check whether a type is this type, or an operand of its nested joint types of a given mode. */
  private def hasJointOperand(mode: JointMode, operand: Type): Boolean =
    type_ == operand || (type_ match
      case TJointType(typeMode, left, right) if typeMode == mode =>
        left.hasJointOperand(mode, operand) || right.hasJointOperand(mode, operand)
      case _ =>
        false
    )

extension (var_ : TypeVar)
  /** Get the bound that replaces a rigid variable at a polarity when splitting a type, which is its
   *  upper bound at the negative polarity, and its lower bound at the positive polarity.
   *
   *  `visited` are the rigid variables that have already been replaced by their bound, including
   *  this one, whose direct occurrences are removed from the bound. */
  private def splitBound(pol: Polarity, visited: Set[TypeVar])(using ctx: SubContext): Type =
    var_.bound(pol.dir).removeDirectVars(visited, pol)

  /** Check whether replacing an occurrence of a rigid variable by its bound loses no derivation,
   *  given the other types of the judgment.
   *
   *  This is the case when the variable occurs in none of them. The judgment is then monotone in the
   *  variable, so that it holds for every type between the bounds of the variable iff it holds for
   *  the bound. Otherwise, the judgment may rely on the occurrences of the variable being the same
   *  type: e.g. `α ≤ α ∨ C` does not hold once its left `α` is replaced by an upper bound `A ∨ B`.
   *
   *  This is also the case when the bounds of the variable are equal, since it is then equal to its
   *  bound, whatever its other occurrences.
   *
   *  This is only an approximation in two cases, in which a derivation may still be lost:
   *  - The occurrences of the variable in the bounds of the other variables of the judgment are not
   *    considered.
   *  - A flexible variable of the judgment may be solved by a type that mentions the variable, which
   *    the replacement prevents: e.g. `β → C ≤ α → C` is solved by `β ≥ α`, but by `β ≥ A ∨ B` once
   *    `α` is replaced by an upper bound `A ∨ B`. Flexible variables do not prevent the replacement,
   *    since flow-sensitive inference relies on the case analysis that it enables, on judgments that
   *    do contain flexible variables. */
  private def isBoundExact(rest: List[Type])(using ctx: SubContext): Boolean =
    !rest.exists(_.hasVar(var_)) || var_.lowerBound == var_.upperBound

extension (type_ : Type)
  /** Split a type in two types by the invertible rule of its polarity, if it can be decomposed as a
   *  union at the negative polarity, or as an intersection at the positive polarity.
   *
   *  The type is decomposed through the equivalences that expose such a joint type:
   *  - The De Morgan laws.
   *  - The distributivity of lambda types over the unions of their parameter and the intersections
   *    of their return type, at the positive polarity.
   *  - The distributivity of joint types over the joint types of the other mode, but only at the
   *    negative polarity and inside lambda types (see `split`).
   *
   *  The type is also decomposed through the bounds of its rigid variables, as long as this is an
   *  equivalence as well given the other side of the judgment, `other` (see `split`). */
  def splitAll(pol: Polarity, other: Type)(using ctx: SubContext, mode: ConstraintMode): Option[(Type, Type)] =
    type_.split(SplitRule.All, pol == Polarity.Negative, List(other))(using ctx, mode, pol, Set())

  /** Split a type in two types by the choice rule of its polarity, if it is an intersection at the
   *  negative polarity, or a union at the positive polarity.
   *
   *  Unlike `splitAll`, only the De Morgan laws are used to expose such a joint type: the types
   *  that the invertible rules can decompose must have been decomposed first, so that there is
   *  nothing left to distribute. Lambda types are not split either, since their intersections are
   *  only decomposed by the invertible rule of the positive polarity.
   *
   *  The type is also decomposed through the bounds of its rigid variables (see `split`). */
  def splitAny(pol: Polarity)(using ctx: SubContext, mode: ConstraintMode): Option[(Type, Type)] =
    type_.split(SplitRule.Any, false, Nil)(using ctx, mode, pol, Set())

  /** Split a type in two types by a rule at a polarity (see `splitAll` and `splitAny`).
   *
   *  `distribute` is whether joint types are distributed over the joint types of the other mode to
   *  expose the joint types that the invertible rule decomposes.
   *
   *  This is the case on the left of a judgment, which is normalised towards a union of
   *  intersections. It is not the case on the right, whose unions are left to the choice rule: once
   *  the left of a judgment is an intersection of types that are not unions, it is compared to the
   *  operands of the unions and intersections of the right as they are. Normalising the right towards
   *  an intersection of unions as well was tried, and made the CTML tests time out: it is exponential
   *  in the number of intersections of the unions of the right, which is the shape of the lower
   *  bounds joined by `joinBounds`.
   *
   *  It is however the case inside the lambda types of both sides. An intersection of lambda types
   *  on the left is compared to a lambda type on the right one lambda type at a time, by the choice
   *  rule, so that the lambda type on the right must first be decomposed as much as possible: e.g.
   *  `(A → B) ∧ (A → C) ≤ A → (D ∨ (B ∧ C))` is only derived from `A → (D ∨ B)` and `A → (D ∨ C)`.
   *
   *  `rest` are the other types of the judgment, which are the other side of the judgment and the
   *  operands of the joint types and lambda types that the type is nested in. They tell whether a
   *  rigid variable may be replaced by its bound by the invertible rules (see below), and are not
   *  used by the choice rules.
   *
   *  `visited` are the rigid variables that have already been replaced by their bound. */
  private def split(rule: SplitRule, distribute: Boolean, rest: List[Type])(using ctx: SubContext, mode: ConstraintMode, pol: Polarity, visited: Set[TypeVar]): Option[(Type, Type)] =
    type_ match
      // A rigid variable is replaced by its bound. Unlike the other cases, this is not an
      // equivalence in general, since the variable itself is forgotten.
      // - The invertible rules only replace a variable when this loses no derivation, which is
      //   mainly when it occurs nowhere else in the judgment (see `isBoundExact`). They used to
      //   replace any variable, which made them lose the derivations that relate its occurrences:
      //   e.g. given an upper bound `A ∨ B` of `α`, `∀β. β → β ≤ α → α` was explored as
      //   `∀β. β → β ≤ A → α` and `∀β. β → β ≤ B → α`, and `α ∧ A ≤ α` as `A ∧ A ≤ α` and
      //   `B ∧ A ≤ α`, which all fail.
      // - The choice rules replace any variable, since they forget an operand anyway.

      // A variable that occurs elsewhere is thus not analysed by cases by the invertible rules:
      // e.g. `(A → C) ∧ (B → C) ≤ α → (α ∨ C)` is not derived. This would require each case to keep
      // the variable, as in `(α ∧ A) → (α ∨ C)` and `(α ∧ B) → (α ∨ C)`, and so to remember that the
      // variable has already been met with its bound, since it could otherwise be split forever.

      // The variables are those that are rigid in the constraining mode (see `isRigidMode`), as for
      // the rules of rigid variables in `subtypeImpl`. The variables that are rigid in the context
      // used to be replaced whatever the mode, including in reconstruction mode, where they are the
      // variables being bounded. Flipping the polarity under negations without fixing this broke the
      // inference of matches (e.g. `foo(int_or_string)` was inferred as `Str` in `ctmlFlow.mls`).
      case TVar(var_) if var_.isRigidMode && !visited.contains(var_) && (rule == SplitRule.Any || var_.isBoundExact(rest)) =>
        val newVisited = visited + var_
        var_.splitBound(pol, newVisited).split(rule, distribute, rest)(using ctx, mode, pol, newVisited)

      // A negated rigid variable is replaced by the negation of its bound at the opposite polarity:
      // e.g. `¬α` is at least `¬τ` when `α` is at most `τ`.
      case TNeg(TVar(var_)) if var_.isRigidMode && !visited.contains(var_) && (rule == SplitRule.Any || var_.isBoundExact(rest)) =>
        val newVisited = visited + var_
        TNeg(var_.splitBound(!pol, newVisited)).split(rule, distribute, rest)(using ctx, mode, pol, newVisited)

      case TNeg(TNeg(body)) =>
        body.split(rule, distribute, rest)

      // A negated joint type is the joint type of the dual mode by the De Morgan laws.
      case TNeg(TJointType(jointMode, left, right)) =>
        TJointType(!jointMode, TNeg(left), TNeg(right)).split(rule, distribute, rest)

      // A negated lambda type is split as the negation of the split of the lambda type, which is at
      // the opposite polarity.
      case TNeg(lam: TLam) =>
        lam.split(rule, distribute, rest)(using ctx, mode, !pol, visited).map((left, right) => (TNeg(left), TNeg(right)))

      case TJointType(jointMode, left, right) if rule.decomposes(jointMode, pol) =>
        Some(left, right)

      // Distribute a joint type over the joint types of the other mode that its operands expose.
      case TJointType(jointMode, left, right) if rule == SplitRule.All && distribute =>
        left.split(rule, distribute, right :: rest) match
          case Some(innerLeft, innerRight) =>
            Some(
              structuralCombine(jointMode, innerLeft,  right),
              structuralCombine(jointMode, innerRight, right),
            )
          case None =>
            right.split(rule, distribute, left :: rest).map((innerLeft, innerRight) =>
              (
                structuralCombine(jointMode, left, innerLeft),
                structuralCombine(jointMode, left, innerRight),
              )
            )

      // Distribute a lambda type over the unions of its parameter, which is at the opposite
      // polarity, and over the intersections of its return type.
      case TLam(param, ret) if rule == SplitRule.All && pol == Polarity.Positive =>
        param.split(rule, true, ret :: rest)(using ctx, mode, !pol, visited) match
          case Some(left, right) =>
            Some(
              TLam(left,  ret),
              TLam(right, ret),
            )
          case None =>
            ret.split(rule, true, param :: rest).map((left, right) =>
              (
                TLam(param, left),
                TLam(param, right),
              )
            )

      case _ =>
        None
