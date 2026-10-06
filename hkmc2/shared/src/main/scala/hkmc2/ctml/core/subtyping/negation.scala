package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.isSubClass
import hkmc2.ctml.core.combine.join

extension (type_ : Type)
  /** Get the simplified negation of this type. */
  def negate(): Type =
    type_.negateStep() match
      case Some(type_) =>
        type_
      case None =>
        TNeg(type_)

  /** Evaluate a negation simplification step if possible. */
  def negateStep(): Option[Type] =
    type_ match
      case TBot =>
        Some(TTop)
      case TTop =>
        Some(TBot)
      case TNeg(body) =>
        Some(body)
      case _ =>
        None

  /** Subtract another type from this type. */
  def subtract(other: Type)(using ctx: SubContext): Type =
    type_.subtractStep(other) match
      case Some(type_) =>
        type_
      case None =>
        TJointType(JointMode.Inter, type_, TNeg(other))

  /** Evaluate a subtraction simplification step if possible: the subtraction `τ ∧ ¬σ` is empty if
   *  `τ ≤ σ` and `σ` admits the complement rules, by non-contradiction `σ ∧ ¬σ ≤ ⊥` (see
   *  `admitsComplement`), it distributes over the operands of a union `τ`, and it is `τ` itself if
   *  `τ` and `σ` are disjoint constructors. */
  def subtractStep(other: Type)(using ctx: SubContext): Option[Type] =
    if other.admitsComplement && checkSubtype(type_, other) then
      return Some(TBot)

    type_ match
      case TJointType(JointMode.Union, left, right) =>
        return Some(join(left.subtract(other), right.subtract(other)))
      case _ =>

    // If the types are disjoint,
    if areDisjointConstructors(type_, other) then
      return Some(type_)

    // Otherwise, return their intersection.
    None

  /** Check whether the complement rules of excluded middle `⊤ ≤ σ ∨ ¬σ` (see `joinMerge`) and
   *  non-contradiction `σ ∧ ¬σ ≤ ⊥` (see `subtractStep`) apply to this type `σ`: to every type with
   *  the `general-complement` option, and to the decided types otherwise (see `isDecided`), as the
   *  `SP-Middle` rule of the mechanization.
   *
   *  Both rules are valid in the set-theoretic model of types, in which every value is a member of
   *  a type or of its negation, so the general complement is sound for the safety of programs. The
   *  restriction is proof-theoretic: the preservation of types through reduction requires every
   *  value to be typed at `σ` or at `¬σ`, since a function typed at both `σ → τ` and `¬σ → τ` is
   *  typed at `⊤ → τ` by distribution over its parameter (see `meetLambdas`), so it accepts every
   *  value, which its body then uses at `σ` or at `¬σ`. No rule types a function at the negation of
   *  a function type, whose membership is a property of its behavior, and no shape decides a type
   *  variable, which is only known through its bounds. */
  def admitsComplement: Boolean =
    config.generalComplement || type_.isDecided

  /** Check whether this type is decided, that is, whether it is a closed Boolean combination of
   *  class types and extrema, which the shape of every value decides: a value is an instance of a
   *  class or it is not, and an instance of a class is typed at the negation of every other class
   *  while a function is typed at the negation of every class (see `areDisjointConstructors`). */
  def isDecided: Boolean =
    type_ match
      case TBot | TTop | TClass(_) =>
        true
      case TNeg(body) =>
        body.isDecided
      case TJointType(_, left, right) =>
        left.isDecided && right.isDecided
      case _ =>
        false
