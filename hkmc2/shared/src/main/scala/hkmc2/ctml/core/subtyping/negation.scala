package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.isSubClass
import hkmc2.ctml.core.combine.{join, meet}
import hkmc2.ctml.core.structural.getVars

// The complement rules.

// Excluded middle `⊤ ≤ σ ∨ ¬σ` (see `joinMerge`) and non-contradiction `σ ∧ ¬σ ≤ ⊥` (see
// `subtractStep`) apply to every type `σ`, as the `S-Compl` and `S-NC` rules of the mechanization,
// so that the negation of a type is its complement.

// Both rules are valid in the set-theoretic model of types, in which a type is a set of values, and
// the soundness of CTML is the safety of programs in that model: a well-typed program does not get
// stuck. They do not preserve types through reduction, which is therefore not a property of CTML:
// `λx. λy. x` is typed at `τ → ⊤ → τ` and at `¬τ → ⊤ → ¬τ`, so at `⊤ → ((⊤ → τ) ∨ (⊤ → ¬τ))` by
// excluded middle at `τ`, and its application to a value `v` reduces to `λy. v`, which is typed at
// that union only if `v` is typed at `τ` or at `¬τ`. No rule decides this for every value when `τ`
// is a function type, whose members are characterized by their behavior.

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
   *  `τ ≤ σ`, by non-contradiction `σ ∧ ¬σ ≤ ⊥` (see the complement rules above), it distributes
   *  over the operands of a union `τ`, and it is `τ` itself if `τ` and `σ` are disjoint
   *  constructors. */
  def subtractStep(other: Type)(using ctx: SubContext): Option[Type] =
    if checkSubtype(type_, other) then
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
 *  `τ ≤ (τ ∧ C) ∨ (τ ∧ ¬C)`. If the judgment fails, the error trees of both cases are added to the
 *  error trees of the judgment derived without cases.
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
