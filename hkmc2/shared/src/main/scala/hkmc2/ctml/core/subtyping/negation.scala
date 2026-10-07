package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.isSubClass
import hkmc2.ctml.core.combine.{join, meet}

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

  /** Get the classes of the Boolean structure of this type, that is, its classes that are not
   *  under a type constructor: through negations, unions, intersections, and the bodies of
   *  constrained types, which notably include the bodies of constraining types. */
  def getBooleanClasses: List[TClass] =
    type_ match
      case type_ : TClass =>
        List(type_)
      case TNeg(body) =>
        body.getBooleanClasses
      case TJointType(_, left, right) =>
        (left.getBooleanClasses ++ right.getBooleanClasses).distinct
      case TConstrained(body, _) =>
        body.getBooleanClasses
      case _ =>
        Nil

/** Derive a subtyping judgment `τ ≤ σ` by cases on a class `C` of the Boolean structure of `σ`, by
 *  excluded middle at `C`: `τ ≤ σ` follows from `τ ∧ C ≤ σ` and `τ ∧ ¬C ≤ σ`, since
 *  `τ ≤ (τ ∧ C) ∨ (τ ∧ ¬C)`. A class is decided, so excluded middle applies to it (see
 *  `admitsComplement`). If the judgment fails, the error trees of both cases are added to the error
 *  trees of the judgment derived without cases.
 *
 *  This derives the judgments whose subtype is split between the operands of a union supertype
 *  without being a union itself, which the choice rules cannot derive, e.g.
 *  `Int ≤ Nat ∨ (Int ∧ ¬Nat)`. Notably, a function that matches a subclass and then its superclass bounds its parameter by a
 *  union of constraining types, one per branch: an argument of the superclass pays the constraint
 *  of the first branch when it is an instance of the subclass, and of the second branch otherwise.
 *
 *  Only a class that splits the subtype is used, that is, one such that neither `τ ≤ C` nor
 *  `τ ≤ ¬C`, so that each case decides one more class of the supertype, and the cases terminate. */
def subtypeCases(sub: Type, sup: Type, error: TypeError)(using ctx: SubContext): SubClauses =
  val splitClass = sup.getBooleanClasses.find((class_) =>
    !checkSubtype(sub, class_) && !checkSubtype(sub, class_.negate())
  )

  splitClass match
    case Some(class_) =>
      try
        ctx.all(
          subtype(meet(sub, class_), sup),
          subtype(meet(sub, class_.negate()), sup),
        )
      catch
        case caseError: TypeError =>
          throw TypeError(None, error.trees ++ caseError.trees)
    case None =>
      throw error
