package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (clauses: Clauses)
  // Merge bounds

  /** Merge two lists of bounds such that they must both be satisfied. */
  def meetBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    // Check if each right bound is satisfied in the left bounds to remove subsumed constraints.
    val filteredRights = clauses.concat(lefts).filterUnsatisfiedBounds(rights)
    // Be careful to check satisfaction against the *filtered* list of constraints to not remove duplicate
    // constraints entirely.
    val filteredLefts = clauses.concat(filteredRights).filterUnsatisfiedBounds(lefts)
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
    val varNames = getBoundedVarsDir(lefts, rights, dir)
    clauses.joinVarsBoundsDir(varNames, lefts, rights, dir)

  /** Get the variables bounded in a given typing direction in either of two bound lists. */
  def getBoundedVarsDir(lefts: List[Bound], rights: List[Bound], dir: Direction): List[String] =
    val typeVars = clauses.typeVars.map(_.name).toList
    val leftVars  = lefts.filterBoundedVars(typeVars, dir)
    val rightVars = rights.filterBoundedVars(typeVars, dir)
    joinVars(leftVars, rightVars)

  /** Get the join bounds of some variables in two lists of constrains in a given typing direction. */
  def joinVarsBoundsDir(varNames: List[String], lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    varNames.map((varName) =>
      val type_ = clauses.joinVarBounds(varName, lefts, rights, dir)
      Bound(varName, dir, type_)
    )

  /** Get the join of the bounds of a variable in two lists of constraints. */
  def joinVarBounds(varName: String, lefts: List[Bound], rights: List[Bound], dir: Direction) =
    given Clauses = clauses
    val leftBounds  = lefts.filterVarDir(varName, dir)
    val rightBounds = rights.filterVarDir(varName, dir)
    val leftBound  = leftBounds.combineMany(dir)
    val rightBound = rightBounds.combineMany(dir)
    val leftType  =
      given Clauses = clauses.append(Bound(varName, dir, leftBound))
      attachConstrainingBounds(leftBound, lefts)
    val rightType =
      given Clauses = clauses.append(Bound(varName, dir, rightBound))
      attachConstrainingBounds(rightBound, rights)
    join(leftType, rightType)

  // Others

  /** Check whether a type variable is a class in the context. */
  def isVarClass(varName: String): Boolean =
    clauses.getTypeVarKind(varName) == TypeVarKind.Class

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(varName: String): Boolean =
    clauses.getTypeVarKind(varName) == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(varName: String): Boolean =
    clauses.getTypeVarKind(varName) == TypeVarKind.Rigid

  /** Retrain the bounds unsatisfied in the context. */
  def filterUnsatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter((bound) => !clauses.checkBoundSatisfied(bound))

  def removeTypeVar(varName: String): Clauses =
    // TODO: Shadowing.
    Clauses(clauses.elems.filter(_ match
      case var_ : TypeVar if var_.name == varName =>
        false
      case bound: Bound if bound.name == varName =>
        false
      case _ =>
        true
    ))

  def compareVarLevels(left: String, right: String): Either[Unit, Unit] =
    val first = clauses.typeVars.findMap((var_) =>
      if var_.name == left then
        Some(Left(()))
      else if var_.name == right then
        Some(Right(()))
      else
        None
    )

    first match
      case Some(either) =>
        either
      case None =>
        throw new TypeError(Some(s"Type variable '${left}' or '${right}' not found in the clauses."))
