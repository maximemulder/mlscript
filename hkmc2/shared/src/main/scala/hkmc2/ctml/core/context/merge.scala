package hkmc2.ctml.core.context

import hkmc2.ctml.core.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (ctx: SubContext)
  // Merge bounds

  /** Merge two lists of bounds such that they must both be satisfied. */
  def meetBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    // Check if each right bound is satisfied in the left bounds to remove subsumed constraints.
    val filteredRights = ctx.extend(lefts).removeSatisfiedBounds(rights)
    // Be careful to check satisfaction against the *filtered* list of constraints to not remove duplicate
    // constraints entirely.
    val filteredLefts = ctx.extend(filteredRights).removeSatisfiedBounds(lefts)
    // Return the concatenation of the filtered bounds.
    filteredLefts ::: filteredRights

  /** Merge two lists of bounds such that either of those must be satisfied. */
  def joinBounds(leftClauses: SubClauses, rightClauses: SubClauses): List[SubClause] =
    val leftTypeDecls = leftClauses.typeVarDecls
    val rightTypeDecls = rightClauses.typeVarDecls
    val fullCtx = ctx.extend(leftTypeDecls.asSubClauses, rightTypeDecls.asSubClauses)
    val lefts = leftClauses.bounds.removeDuplicateBounds()
    val rights = rightClauses.bounds.removeDuplicateBounds()
    val lowerBounds = fullCtx.joinBoundsDir(lefts, rights, Direction.Sub)
    val upperBounds = fullCtx.joinBoundsDir(lefts, rights, Direction.Super)
    lowerBounds ::: upperBounds ::: leftTypeDecls ::: rightTypeDecls

  /** Join two lists of bounds in a given typing direction. */
  def joinBoundsDir(lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    val vars = getBoundedVarsDir(lefts, rights, dir)
    ctx.joinVarsBoundsDir(vars, lefts, rights, dir)

  /** Get the variables bounded in a given typing direction in either of two bound lists. */
  def getBoundedVarsDir(lefts: List[Bound], rights: List[Bound], dir: Direction): List[TypeVar] =
    val typeVars = ctx.clauses.typeVarDecls.map(_.var_).toList
    val leftVars  = lefts.filterBoundedVars(typeVars, dir)
    val rightVars = rights.filterBoundedVars(typeVars, dir)
    leftVars.concatAllUnique(rightVars)

  /** Get the join bounds of some variables in two lists of constrains in a given typing direction. */
  def joinVarsBoundsDir(vars: List[TypeVar], lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    vars.map(var_ =>
      val type_ = ctx.joinVarBounds(var_, lefts, rights, dir)
      Bound(var_, dir, type_)
    )

  /** Get the join of the bounds of a variable in two lists of constraints. */
  def joinVarBounds(var_ : TypeVar, lefts: List[Bound], rights: List[Bound], dir: Direction) =
    given SubContext = ctx
    val leftBound  = lefts.getVarDirType(var_, dir)
    val rightBound = rights.getVarDirType(var_, dir)
    val leftCtx  = ctx.extend(Bound(var_, dir, leftBound))
    val rightCtx = ctx.extend(Bound(var_, dir, rightBound))
    val filteredLefts  = leftCtx
      .removeSatisfiedBounds(lefts)
      .removeDuplicateBounds()
      .sortBounds()(using leftCtx)
      .map(_.toConstraint)
    val filteredRights = rightCtx
      .removeSatisfiedBounds(rights)
      .removeDuplicateBounds()
      .sortBounds()(using rightCtx)
      .map(_.toConstraint)
    // The bound of each branch is guarded by the other constraints of the branch. When the guard of
    // a branch does not hold, its guarded bound should act as the identity of the joint that
    // combines the branch bounds, so that only the other branch remains. Upper bounds are thus
    // guarded by constraining types, which act as `⊥`, the identity of their union, and lower bounds
    // by constrained types, which act as `⊤`, the identity of their intersection (this corresponds
    // to the paper's dual context join).
    // Both choices are forced, since the other kind of guarded type acts as the absorbing element of
    // the joint, and so erases the bounds of the other branch. This was the case of two merge modes
    // that used to be configurable:
    // - The constrained mode guarded upper bounds by constrained types as well. An upper bound
    //   `({Δl} ⟹ Int) ∨ ({Δr} ⟹ Str)` was then `⊤` as soon as either guard did not hold. Notably, a
    //   branch that did not bound a variable from above contributed `{Δ} ⟹ ⊤`, which is `⊤` whatever
    //   `Δ`, so that the variable was not bounded from above at all. This was the default mode, in
    //   which e.g. the parameter `b` of `baz` in `ctmlLet.mls` had no upper bound.
    // - The constraining mode used to guard lower bounds by constraining types as well. Since they
    //   are encoded as negated constrained types, they can be eliminated without proving their guard,
    //   so that a lower bound `¬({Δl} ⟹ ¬Int) ∧ ¬({Δr} ⟹ ¬Str)` let a variable be used as `Int` even
    //   when `Δl` does not hold.
    val (leftType, rightType) = dir match
      case Direction.Sub =>
        (
          makeConstrainingType(leftBound, filteredLefts),
          makeConstrainingType(rightBound, filteredRights),
        )
      case Direction.Super =>
        (
          makeConstrainedType(leftBound, filteredLefts),
          makeConstrainedType(rightBound, filteredRights),
        )

    hkmc2.ctml.core.combine.combine(!dir.jointMode, leftType, rightType)
