package hkmc2.ctml.core.context

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

extension (ctx: Context)
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
  def joinBounds(leftClauses: Clauses, rightClauses: Clauses): List[Bound] =
    val lefts = leftClauses.bounds
    val rights = rightClauses.bounds
    val lowerBounds = joinBoundsDir(lefts, rights, Direction.Sub)
    val upperBounds = joinBoundsDir(lefts, rights, Direction.Super)
    lowerBounds ::: upperBounds

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
    given Context = ctx
    val leftBound  = lefts.getVarDirType(var_, dir)
    val rightBound = rights.getVarDirType(var_, dir)
    config.mergeMode match
      case MergeMode.Constraining =>
        val leftType =
          val leftCtx = ctx.extend(Bound(var_, dir, leftBound))
          given Context = leftCtx
          val filteredLefts = leftCtx.removeSatisfiedBounds(lefts).map(_.toConstraint)
          makeConstrainingType(leftBound, filteredLefts)
        val rightType =
          val rightCtx = ctx.extend(Bound(var_, dir, rightBound))
          given Context = rightCtx
          val filteredRights = rightCtx.removeSatisfiedBounds(rights).map(_.toConstraint)
          makeConstrainingType(rightBound, filteredRights)
        hkmc2.ctml.core.combine.combine(leftType, rightType, dir.invert())(using cache = SubtypingCache())
      case MergeMode.Constrained =>
        val leftType =
          val leftCtx = ctx.extend(Bound(var_, dir, leftBound))
          given Context = leftCtx
          val filteredLefts = leftCtx.removeSatisfiedBounds(lefts).map(_.toConstraint)
          makeConstrainedType(leftBound, filteredLefts)
        val rightType =
          val rightCtx = ctx.extend(Bound(var_, dir, rightBound))
          given Context = rightCtx
          val filteredRights = rightCtx.removeSatisfiedBounds(rights).map(_.toConstraint)
          makeConstrainedType(rightBound, filteredRights)
        hkmc2.ctml.core.combine.combine(leftType, rightType, dir.invert())(using cache = SubtypingCache())
