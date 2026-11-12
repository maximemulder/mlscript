package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*

/** The subtyping judgments in progress on the current path of the subtyping search whose side is a
 *  type variable, which are the judgments that the variable rules resolve through the bounds of
 *  that variable (see `subtypeImpl`).
 *
 *  Resolving a judgment through a bound reads the bound from the context rather than from the
 *  judgment, so that, unlike the other rules, it does not decrease the size of the judgment, and
 *  may repeat a judgment in progress through a cycle of bounds: e.g. `A ≤ Str` is resolved as
 *  `B ≤ Str` given `A ≤ B ∨ Str`, and back as `A ≤ Str` given `B ≤ A ∨ Str`. A judgment
 *  that repeats one in progress is thus not resolved again, but answered from the trail (see
 *  `subtypeTrail`).
 *
 *  The trail is passed down the search and never returned, so that it describes the current path
 *  of the search rather than the whole search: a judgment that repeats one of another branch of the
 *  search is resolved again, in the context of that branch.
 *
 *  The exact judgments are compared syntactically, without their contexts. Flexible repeats use
 *  the pending-bound rule described in `subtypeTrail`. Checking opposite bounds before recording
 *  a flexible bound avoids some self-application loops, but does not bound fresh instantiation.
 *
 *  The separate provenance keys recognize paths up to the origins of newly opened variables and
 *  a bounded description of their goals and side bounds. Input variables are anchored at each
 *  subtyping entry, not at the enclosing inference entry: constructor instances created while
 *  inferring an argument are already distinct inputs when that argument is checked. A collision
 *  reports an inconclusive search; unlike an exact flexible repeat, it never supplies a proof or
 *  discards an obligation. These keys omit parts of the context and cannot cache completed proofs.
 *
 *  The separate `instances` set tracks target-directed universal witnesses, which can repeat
 *  without a variable side. A repeated witness is skipped rather than used as a hypothesis. */
case class SubtypingTrail(
  /** The judgments in progress, as pairs of a subtype and a supertype. */
  val judgments: Set[(Type, Type)] = Set(),
  /** Target-directed universal instances being checked on this path. A revisit skips that
   *  witness; it does not establish the judgment. */
  val instances: Set[(Type, Type)] = Set(),
  /** Stable binder marks opened along this search path. */
  flow: Set[VarOrigin] = Set.empty,
  /** Resource accounting shared by descendants of one inference or subtyping operation. */
  search: Option[SearchSession] = None,
  /** Variables present at entry to this subtyping search, including instances already created by
   *  argument inference. These are anchors, even when the enclosing inference session is older. */
  roots: Option[Set[TypeVar]] = None,
  /** Abstract variable-side judgments on this path; collisions do not discharge constraints. */
  provenanceKeys: Set[ProvenanceKey] = Set.empty,
  /** A budget for nested optional queries; absent in required constraint solving. */
  checking: Option[CheckingBudget] = None,
  /** A closure's encoded carrier is checked by ordinary rules, without closing it again. */
  closingRecursiveInstance: Boolean = false,
):
  /** Check whether a judgment repeats a judgment in progress. */
  def contains(sub: Type, sup: Type): Boolean =
    this.judgments.contains((sub, sup))

  /** Record a judgment as in progress if a variable rule may resolve it, that is, if one of its
   *  sides is a type variable or a negated type variable (see `lookupVar`). */
  def add(sub: Type, sup: Type): SubtypingTrail =
    if sub.lookupVar.isDefined || sup.lookupVar.isDefined then
      this.copy(judgments = this.judgments + ((sub, sup)))
    else
      this

extension (type_ : Type)
  /** Get the variable of a type that is a type variable or a negated type variable, through the
   *  bounds of which the variable rules resolve a judgment whose side is this type: a negated
   *  variable is replaced by the negation of its bound in the opposite direction (see `split`), or
   *  is moved to the other side of the judgment by contraposition (see `subtypeImpl`). */
  def lookupVar: Option[TypeVar] =
    type_ match
      case TVar(var_) =>
        Some(var_)
      case TNeg(TVar(var_)) =>
        Some(var_)
      case _ =>
        None
