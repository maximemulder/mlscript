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
    // Only the asserted bounds of the branches are joined, since their effective bounds may rely
    // on the assumptions of their branch (see `BoundKind`).
    val lefts = leftClauses.assertedBounds
    val rights = rightClauses.assertedBounds
    val upperBounds = fullCtx.joinBoundsDir(lefts, rights, Direction.Sub)
    val lowerBounds = fullCtx.joinBoundsDir(lefts, rights, Direction.Super)
    upperBounds ::: lowerBounds ::: leftTypeDecls ::: rightTypeDecls

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
      Bound(var_, dir, type_, BoundKind.Asserted)
    )

  /** Get the join of the bounds of a variable in two lists of constraints. */
  def joinVarBounds(var_ : TypeVar, lefts: List[Bound], rights: List[Bound], dir: Direction) =
    given SubContext = ctx
    val (leftBound, filteredLefts) = ctx.getBranchVarBound(var_, lefts, dir)
    val (rightBound, filteredRights) = ctx.getBranchVarBound(var_, rights, dir)
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

  /** Get the bound of a variable in the bounds of a branch, and the constraints of the branch that
   *  guard it. */
  def getBranchVarBound(var_ : TypeVar, bounds: List[Bound], dir: Direction): (Type, List[Constraint]) =
    // A variable may have several bounds in the same direction in a branch, e.g. the bound of the
    // pattern of a match and the bound of a nested match, which must all be kept, both in the bound
    // of the variable and in the guards of the other variables. Taking only one of them, as was
    // done before (each variable used to have a single relevant bound), lost the other ones.
    val bound = bounds.getVarDirType(var_, dir)
    val boundCtx = ctx.extend(Bound(var_, dir, bound, BoundKind.Asserted))
    // The guard is made of the other bounds of the branch, except those that hold given the bound of
    // the variable. The bound of the variable itself is removed syntactically, rather than by
    // checking that it holds, which may fail: e.g. a rigid variable is not compared to a
    // constraining type using its bound, since the constraining type is contraposed first, so that
    // the guard of a variable used to contain its own bound.
    val guard = boundCtx
      .removeSatisfiedBounds(bounds.combineVarBounds().filterNot(_.isTypeVarDirBound(var_, dir)))
      .sortBounds()(using boundCtx)
      .map(_.toConstraint)
    (bound, guard)
