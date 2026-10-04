package hkmc2.ctml.core.clauses

import scala.collection.mutable.Set as MutSet

import hkmc2.ctml.core.*
import hkmc2.ctml.core.context.extend
import hkmc2.ctml.types.*

// Operations on the bounds of type variables that depend on the kind of the bounds.

// Bounds are either asserted or effective (see `BoundKind`). The operations defined here are the
// only ones that depend on the kind of the bounds: the bound of a variable is read with `varBound`,
// the bounds that are output (e.g. by context joins or by quantification) are the asserted bounds,
// and effective bounds are only created by `makeBoundClauses` and `overrideVarBounds`, and removed
// by `removeEffectiveBounds` and `summarizeBounds`. Other operations on bounds handle all bounds
// uniformly.

extension (clauses: AsSubClauses)
  /** Get the bound of a type variable in a type direction defined in the clauses, if any.
   *
   *  The bound is the combination of the most recent effective bound of the variable in that
   *  direction, which subsumes the bounds that precede it, and of the asserted bounds that follow
   *  it, or of all the asserted bounds if there is no effective bound (see `BoundKind`). */
  def varBound(var_ : TypeVar, dir: Direction): Option[Type] =
    // The bounds of the variable in that direction, from the most recent to the oldest.
    val bounds = clauses.iterator.flatMap(_ match
      case bound: Bound if bound.var_ == var_ && bound.dir == dir =>
        Some(bound)
      case _ =>
        None
    )
    val (assertedBounds, otherBounds) = bounds.span(_.kind == BoundKind.Asserted)
    val assertedTypes = assertedBounds.map(_.type_).toList
    val effectiveTypes = otherBounds.take(1).map(_.type_).toList
    (assertedTypes ::: effectiveTypes).reduceRightOption(makeJointType(dir.jointMode, _, _))

  /** Get the asserted bounds defined in the clauses. */
  def assertedBounds: List[Bound] =
    clauses.iterator.assertedBounds.toList

  /** Get the asserted bounds of a type variable defined in the clauses. */
  def varAssertedBounds(var_ : TypeVar): List[Bound] =
    clauses.iterator.typeVarClauses(var_).typeVarAssertedBounds(var_).toList

  /** Check whether the clauses contain effective bounds. */
  def hasEffectiveBounds: Boolean =
    clauses.iterator.exists(_ match
      case bound: Bound =>
        bound.kind == BoundKind.Effective
      case _ =>
        false
    )

extension (clauses: Iterator[SubClause])
  /** Iterate over the asserted bounds defined in the clauses. */
  def assertedBounds: Iterator[Bound] =
    clauses.flatMap(_ match
      case bound: Bound if bound.kind == BoundKind.Asserted =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the asserted bounds of a variable in the clauses. */
  def typeVarAssertedBounds(var_ : TypeVar): Iterator[Bound] =
    clauses.assertedBounds.filter(_.var_ == var_)

extension (clauses: SubClauses)
  /** Remove the effective bounds of the clauses, which must be done when the clauses leave the
   *  scope in which these effective bounds were computed (see `BoundKind`). */
  def removeEffectiveBounds(): SubClauses =
    clauses.filterBounds(_.kind == BoundKind.Asserted)

  /** Replace the bounds of the clauses by a single asserted bound for each type variable and
   *  direction, which is the bound of the variable in the clauses (see `varBound`). This bound takes
   *  the place of the most recent bound of its variable and direction, which follows the
   *  declarations of the variables of all the bounds it summarizes. */
  def summarizeBounds(): SubClauses =
    val summarized = MutSet[(TypeVar, Direction)]()
    SubClauses(clauses.elems.flatMap(_ match
      case bound: Bound =>
        if summarized.add((bound.var_, bound.dir)) then
          clauses.varBound(bound.var_, bound.dir).map(Bound(bound.var_, bound.dir, _, BoundKind.Asserted))
        else
          None
      case clause =>
        Some(clause)
    ))

extension (ctx: SubContext)
  /** Extend the context with new upper and lower bounds of a type variable that override its
   *  current bounds. Since they are effective bounds, they only hold in this context, and are not
   *  output. */
  def overrideVarBounds(var_ : TypeVar, upperBound: Type, lowerBound: Type): SubContext =
    ctx.extend(
      Bound(var_, Direction.Sub, upperBound, BoundKind.Effective),
      Bound(var_, Direction.Super, lowerBound, BoundKind.Effective),
    )

/** Make the clauses that bound a type variable by a new type in a given direction: the new type is
 *  asserted as a bound of the variable, and the effective type, which must be the combination of
 *  the current bound of the variable and the new type in the current scope, becomes its effective
 *  bound (see `BoundKind`). */
def makeBoundClauses(var_ : TypeVar, dir: Direction, type_ : Type, effectiveType: Type): SubClauses =
  SubClauses(List(
    Bound(var_, dir, effectiveType, BoundKind.Effective),
    Bound(var_, dir, type_, BoundKind.Asserted),
  ))
