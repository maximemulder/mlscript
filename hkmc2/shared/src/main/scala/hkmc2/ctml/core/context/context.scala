package hkmc2.ctml.core.context

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (ctx: Context)
  // Merge bounds

  /** Merge two lists of bounds such that they must both be satisfied. */
  def meetBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    // Check if each right bound is satisfied in the left bounds to remove subsumed constraints.
    val filteredRights = ctx.extend(lefts).filterUnsatisfiedBounds(rights)
    // Be careful to check satisfaction against the *filtered* list of constraints to not remove duplicate
    // constraints entirely.
    val filteredLefts = ctx.extend(filteredRights).filterUnsatisfiedBounds(lefts)
    // Return the concatenation of the filtered bounds.
    filteredLefts ::: filteredRights

  /** Merge two lists of bounds such that either of those must be satisfied. */
  def joinBounds(leftClauses: Clauses, rightClauses: Clauses): List[Bound] =
    val lefts = leftClauses.bounds.toList
    val rights = rightClauses.bounds.toList
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
    joinVars(leftVars, rightVars)

  /** Get the join bounds of some variables in two lists of constrains in a given typing direction. */
  def joinVarsBoundsDir(vars: List[TypeVar], lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    vars.map(var_ =>
      val type_ = ctx.joinVarBounds(var_, lefts, rights, dir)
      Bound(var_, dir, type_)
    )

  /** Get the join of the bounds of a variable in two lists of constraints. */
  def joinVarBounds(var_ : TypeVar, lefts: List[Bound], rights: List[Bound], dir: Direction) =
    given Context = ctx
    val leftBounds  = lefts.filterVarDir(var_, dir)
    val rightBounds = rights.filterVarDir(var_, dir)
    val leftBound  = leftBounds.combineMany(dir)
    val rightBound = rightBounds.combineMany(dir)
    val leftType  =
      given Context = ctx.extend(Bound(var_, dir, leftBound))
      attachConstrainingBounds(leftBound, lefts)
    val rightType =
      given Context = ctx.extend(Bound(var_, dir, rightBound))
      attachConstrainingBounds(rightBound, rights)
    join(leftType, rightType)

  // Others

  /** Check whether a type variable is a class in the context. */
  def isVarClass(var_ : TypeVar): Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Class

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(var_ : TypeVar): Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(var_ : TypeVar): Boolean =
    ctx.getTypeVarKind(var_) == TypeVarKind.Rigid

  /** Retrain the bounds unsatisfied in the context. */
  def filterUnsatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter((bound) => !ctx.checkBoundSatisfied(bound))

  def compareVarLevels(left: TypeVar, right: TypeVar): Either[Unit, Unit] =
    val first = ctx.clauses.typeVarDecls.findMap(decl =>
      if decl.var_ == left then
        Some(Left(()))
      else if decl.var_ == right then
        Some(Right(()))
      else
        None
    )

    first match
      case Some(either) =>
        either
      case None =>
        throw new TypeError(Some(s"Type variable '${left}' or '${right}' not found in the clauses."))
