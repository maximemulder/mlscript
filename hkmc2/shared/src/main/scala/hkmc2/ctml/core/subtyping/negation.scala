package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.{isSubClass, makeJointType}
import hkmc2.ctml.core.combine.{join, meet}
import hkmc2.ctml.core.structural.{getVars, hasVar, sameTypeSyntax, TypeSyntaxKey}

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

    (type_, other) match
      case (TField(leftName, left), TField(rightName, right)) if leftName == rightName =>
        return Some(TField(leftName, left.subtract(right)))
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

  /** Check whether a class may split this type `τ`: neither `τ ≤ C` nor `τ ≤ ¬C` was proved.
   *  An inconclusive query does not justify attempting more case splits; the type may already
   *  lie entirely in one case, so splitting again could reproduce the same search. */
  def isSplitBy(class_ : TClass)(using ctx: SubContext): Boolean =
    checkSubtypeResult(type_, class_) == SubtypeCheck.NotProved &&
      checkSubtypeResult(type_, class_.negate()) == SubtypeCheck.NotProved

extension (constraint: Constraint)
  /** Get the bound imposed on a variable in the given direction, in either orientation. */
  private def boundOf(var_ : TypeVar, dir: Direction): Option[Type] =
    (constraint.left, constraint.right) match
      case (TVar(`var_`), bound) if constraint.dir == dir => Some(bound)
      case (bound, TVar(`var_`)) if constraint.dir != dir => Some(bound)
      case _ => None

/** Classes tested by guards on the parameter in a Boolean combination of result types. */
private def resultGuardClasses(result: Type, param: TypeVar): List[TClass] = result match
  case TConstrained(body, guard) =>
    guard.boundOf(param, Direction.Sub).toList.flatMap(_.getBooleanClasses) :::
      resultGuardClasses(body, param)
  case TJointType(_, left, right) =>
    resultGuardClasses(left, param) ::: resultGuardClasses(right, param)
  case TNeg(body) => resultGuardClasses(body, param)
  case _ => Nil

/** Each matching promise proves P → R by instantiation at its own test P. An exhaustive
 *  pair covers the entire domain. When every arm matches, subtract their whole union so a
 *  later caller can still use any promised input. */
private def coveredDomain(promises: Type, param: TypeVar, result: Type): Option[Type] =
  val arms = promises.exactComponents(JointMode.Inter)
  val tests = arms.flatMap:
    case TConstrained(value, condition) if sameTypeSyntax(value, result) =>
      condition.boundOf(param, Direction.Sub).filterNot(_.hasVar(param))
    case _ => None
  val present = tests.iterator.map(TypeSyntaxKey(_)).toSet
  val exhaustive = tests.exists:
    case TTop => true
    case TNeg(p) => present(TypeSyntaxKey(p))
    case _ => false
  if exhaustive then Some(TTop)
  else if tests.size == arms.size && tests.size >= 2 then
    Some(tests.reduceLeft(TJointType(JointMode.Union, _, _)))
  else None

/** For a source S known to satisfy U → R, checking S ≤ D → R reduces exactly to checking
 *  S ≤ (D ∧ ¬U) → R. A top cover discharges the judgment for every assignment to D. */
def uncoveredArrowTarget(source: TUniv, target: TLam)(using SubContext): Option[Type] =
  (source.body, target.param) match
    case (TLam(TVar(a), arms @ TJointType(JointMode.Inter, _, _)), TVar(d))
        if a == source.var_ && d.isFlex && !target.ret.hasVar(a) =>
      coveredDomain(arms, a, target.ret).map:
        case TTop => TTop
        case u => TLam(TJointType(JointMode.Inter, target.param, TNeg(u)), target.ret)
    case _ => None

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

/** Derive `∀ᾱ. {c̄} ⟹ α → σ ≤ τ → ρ` by checking a separate instance at `τ ∧ C` and `τ ∧ ¬C`.
 *  The two domains cover τ by excluded middle, and arrows distribute over a union of domains.
 *  Parameter bounds and branch guards supply the candidate classes C.
 *
 *  The universal type is thus instantiated once per case, as it is for an argument that is a
 *  union, whose lambda type the invertible rules decompose into an intersection (see `splitAll`).
 *  A single instance must instead pay the constraints of several operands of the union of
 *  constraining types that bounds its parameter (see `subtypeCases`), so that the guards of the
 *  promises of its result do not hold: e.g. against a function that matches `Nat` and then `Int`,
 *  an instance at `Int` gives `(({Int ≤ Nat} ⟹ Str) ∧ ({Int ≤ Int ∧ ¬Nat} ⟹ Int)) ∨ Str ∨ Int`,
 *  while an instance at `Nat` gives `Str`, and an instance at `Int ∧ ¬Nat` gives `Int`.
 *
 *  A flexible argument's lower bound can witness several alternatives. It guides the choice of
 *  partitions, while the proof covers the entire argument, including values outside that bound. */
def subtypeUnivCases(sub: TUniv, sup: TLam)(using ctx: SubContext): Option[SubClauses] =
  val (_, body) = sub.getUnivComponents
  val (lambda, constraints) = body.getConstrainedComponents
  val (param, ret) = lambda match
    case TLam(TVar(param), ret) => (param, ret)
    case _ => return None

  val domain = sup.param
  val domains = domain match
    case TVar(v) if v.isFlex =>
      val alternatives = v.lowerBound.exactComponents(JointMode.Union)
      if alternatives.size < 2 then return None
      val results = ret match
        case TVar(r) => constraints.flatMap(_.boundOf(r, Direction.Super))
        case _ => List(ret)
      val classes = results.flatMap(resultGuardClasses(_, param)).distinct
      partitionDomain(domain, alternatives, classes)
    case _ if domain.getVars.exists(_.isFlex) => return None
    case _ =>
      val classes = constraints.flatMap(_.boundOf(param, Direction.Sub))
        .flatMap(_.getBooleanClasses)
      classes.distinct.find(domain.isSplitBy(_)) match
        case Some(pivot) => List(meet(domain, pivot), meet(domain, pivot.negate()))
        case None => return None

  Option.when(domains.size > 1):
    domains.foldLeft(SubClauses.empty): (clauses, part) =>
      ctx.seqUnit(subtype(sub, TLam(part, sup.ret)), clauses)

/** Partition the domain by excluded middle only where a result guard distinguishes alternatives
 *  of its known lower bound. Each recursive split has fewer witnesses in each branch. Unknown
 *  alternatives still belong to the full domain covered by the two complementary intersections. */
private def partitionDomain(domain: Type, alternatives: List[Type], classes: List[TClass])
    (using SubContext): List[Type] = classes match
  case pivot :: rest if alternatives.size >= 2 =>
    val (inside, others) = alternatives.partition(checkSubtype(_, pivot))
    val outside = others.filter(checkSubtype(_, pivot.negate()))
    if inside.isEmpty || outside.isEmpty then partitionDomain(domain, alternatives, rest)
    else
      partitionDomain(makeJointType(JointMode.Inter, domain, pivot), inside, rest) :::
        partitionDomain(makeJointType(JointMode.Inter, domain, pivot.negate()), outside, rest)
  case _ => List(domain)
