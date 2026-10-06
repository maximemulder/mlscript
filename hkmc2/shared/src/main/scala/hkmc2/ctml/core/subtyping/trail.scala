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
 *  This is the trail of the paper's algorithm restricted to exact repeats. The judgments are
 *  compared syntactically, in particular without their context, so that a judgment also repeats
 *  one that was resolved in a context with fewer bounds, which the paper tells apart by the keys of
 *  its trail. The paper also defers the judgments that repeat one in progress up to the variables
 *  created by the search, which cuts the chain of instantiations of self-application: a universal
 *  type recorded as a bound of a variable is checked against the opposite bounds of the variable,
 *  which contain an instance of the type, and this check opens another instance. This trail does
 *  not need to: a new bound of a flexible variable is checked against the opposite bounds of the
 *  variable before being recorded (see `subtypeFlexVar`), so that the check does not meet the type
 *  being recorded. */
case class SubtypingTrail(
  /** The judgments in progress, as pairs of a subtype and a supertype. */
  val judgments: Set[(Type, Type)] = Set(),
):
  /** Check whether a judgment repeats a judgment in progress. */
  def contains(sub: Type, sup: Type): Boolean =
    this.judgments.contains((sub, sup))

  /** Record a judgment as in progress if a variable rule may resolve it, that is, if one of its
   *  sides is a type variable or a negated type variable (see `lookupVar`). */
  def add(sub: Type, sup: Type): SubtypingTrail =
    if sub.lookupVar.isDefined || sup.lookupVar.isDefined then
      SubtypingTrail(this.judgments + ((sub, sup)))
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
