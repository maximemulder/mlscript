package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.isSubClass
import hkmc2.ctml.core.combine.{join, meet}
import hkmc2.ctml.core.structural.getVars

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

  /** Check whether a class splits this type `τ`, that is, whether neither `τ ≤ C` nor `τ ≤ ¬C`, so
   *  that both `τ ∧ C` and `τ ∧ ¬C` are smaller than `τ`. */
  def isSplitBy(class_ : TClass)(using ctx: SubContext): Boolean =
    !checkSubtype(type_, class_) && !checkSubtype(type_, class_.negate())

extension (constraint: Constraint)
  /** Get the upper bound of a type variable that this constraint is, if any. */
  def getUpperBound(var_ : TypeVar): Option[Type] =
    (constraint.left, constraint.dir, constraint.right) match
      case (TVar(`var_`), Direction.Sub, bound) =>
        Some(bound)
      case (bound, Direction.Super, TVar(`var_`)) =>
        Some(bound)
      case _ =>
        None

/** Derive a subtyping judgment `τ ≤ σ` by cases on a class `C` of the Boolean structure of `σ`, by
 *  excluded middle at `C`: `τ ≤ σ` follows from `τ ∧ C ≤ σ` and `τ ∧ ¬C ≤ σ`, since
 *  `τ ≤ (τ ∧ C) ∨ (τ ∧ ¬C)`. A class is decided, so excluded middle applies to it (see
 *  `admitsComplement`). If the judgment fails, the error trees of both cases are added to the error
 *  trees of the judgment derived without cases.
 *
 *  This derives the judgments whose subtype is split between the operands of a union supertype
 *  without being a union itself, which the choice rules cannot derive, e.g.
 *  `Int ≤ Nat ∨ (Int ∧ ¬Nat)`. Notably, a function that matches a subclass and then its superclass
 *  bounds its parameter by a union of constraining types, one per branch: an argument of the
 *  superclass pays the constraint of the first branch when it is an instance of the subclass, and
 *  of the second branch otherwise (see also `subtypeUnivCases`).
 *
 *  Only a class that splits the subtype is used (see `isSplitBy`), so that each case decides one
 *  more class of the supertype, and the cases terminate. */
def subtypeCases(sub: Type, sup: Type, error: TypeError)(using ctx: SubContext): SubClauses =
  sup.getBooleanClasses.find(sub.isSplitBy(_)) match
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

/** Derive a subtyping judgment `∀ᾱ. {c̄} ⟹ α → σ ≤ τ → ρ` by cases on a class `C` of the upper
 *  bounds of its parameter `α` in `c̄`, by excluded middle at `C` and the distributivity of lambda
 *  types over the unions of their parameter: from `∀ᾱ. {c̄} ⟹ α → σ ≤ (τ ∧ C) → ρ` and
 *  `∀ᾱ. {c̄} ⟹ α → σ ≤ (τ ∧ ¬C) → ρ`. Get `None` if no such class splits `τ` (see `isSplitBy`).
 *
 *  The universal type is thus instantiated once per case, as it is for an argument that is a
 *  union, whose lambda type the invertible rules decompose into an intersection (see `splitAll`).
 *  A single instance must instead pay the constraints of several operands of the union of
 *  constraining types that bounds its parameter (see `subtypeCases`), so that the guards of the
 *  promises of its result do not hold: e.g. against a function that matches `Nat` and then `Int`,
 *  an instance at `Int` gives `(({Int ≤ Nat} ⟹ Str) ∧ ({Int ≤ Int ∧ ¬Nat} ⟹ Int)) ∨ Str ∨ Int`,
 *  while an instance at `Nat` gives `Str`, and an instance at `Int ∧ ¬Nat` gives `Int`.
 *
 *  An argument with flexible variables is not split, since these variables are bounded by the
 *  parameter instead, which carries the case analysis to the function that binds them. */
def subtypeUnivCases(sub: TUniv, sup: TLam)(using ctx: SubContext): Option[SubClauses] =
  if sup.param.getVars.exists(_.isFlex) then
    return None

  val (_, body) = sub.getUnivComponents
  val (lambda, constraints) = body.getConstrainedComponents
  val classes = lambda match
    case TLam(TVar(param), _) =>
      constraints.flatMap(_.getUpperBound(param)).flatMap(_.getBooleanClasses).distinct
    case _ =>
      Nil

  classes.find(sup.param.isSplitBy(_)).map((class_) =>
    ctx.all(
      subtype(sub, TLam(meet(sup.param, class_), sup.ret)),
      subtype(sub, TLam(meet(sup.param, class_.negate()), sup.ret)),
    )
  )
