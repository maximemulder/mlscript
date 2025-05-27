package hkmc2.ctml.core

import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

extension (clauses: Clauses)
  /** Join two lists of variables by removing duplicates. */
  def joinVars(lefts: List[String], rights: List[String]): List[String] =
    val filteredRights = rights.filter((right) => !(lefts.exists ((left) => left == right)))
    lefts ::: filteredRights

  // Merge bounds

  /** Merge two lists of bounds such that they must both be satisfied. */
  def meetBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    // Check if each right bound is satisfied in the left bounds to remove subsumed constraints.
    val filteredRights = clauses.addElems(lefts).filterUnsatisfiedBounds(rights)
    // Be careful to check satisfaction against the *filtered* list of constraints to not remove duplicate
    // constraints entirely.
    val filteredLefts = clauses.addElems(filteredRights).filterUnsatisfiedBounds(lefts)
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
    val leftBounds  = lefts.collectVarBounds(varName, dir)
    val rightBounds = rights.collectVarBounds(varName, dir)
    val leftBound  = combineMany(leftBounds,  dir)
    val rightBound = combineMany(rightBounds, dir)
    val leftType  =
      given Clauses = clauses.addClause(Bound(varName, dir, leftBound))
      attachConstrainingBounds(leftBound, lefts)
    val rightType =
      given Clauses = clauses.addClause(Bound(varName, dir, rightBound))
      attachConstrainingBounds(rightBound, rights)
    join(leftType, rightType)

  // Combinators

  /** Evaluate all the given functions and meet their returned bounds. */
  def all(fs: (() => Clauses)*): Clauses =
    Clauses(fs.flatMap(_().elems).toList)

  /** Evaluate all the given functions and join their returned bounds. */
  def any(fs: (() => Clauses)*): Clauses =
    val result = fs.foldRight(None: Option[Clauses])((f, result) =>
      try
        val bounds = f()
        result match
          case Some(resultBounds) =>
            Some(Clauses(joinBounds(resultBounds, bounds)))
          case None =>
            Some(bounds)
      catch
        case error : TypeError =>
          result
    )

    result match
      case Some(bounds) =>
        bounds
      case None =>
        throw new TypeError()

  // Others

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(varName: String): Boolean =
    clauses.getTypeVarKind(varName) == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(varName: String): Boolean =
    clauses.getTypeVarKind(varName) == TypeVarKind.Rigid

  /** Get the lower bound of a type variable in the context. */
  def getVarLowerBound(varName: String): Type =
    clauses.varBounds(varName)
      .filter(_.dir == Direction.Super)
      .foldRight(TBot: Type)((bound, type_) =>
        given Clauses = clauses
        join(type_, bound.type_)
      )

  /** Get the upper bound of a type variable in the context. */
  def getVarUpperBound(varName: String): Type =
    clauses.varBounds(varName)
      .filter(_.dir == Direction.Sub)
      .foldRight(TTop: Type)((bound, type_) =>
        given Clauses = clauses
        meet(type_, bound.type_)
      )

  /** Extract a variable and its bounds from the context. */
  def extractVarBounds(varName: String): (List[Clause], (Type, Type)) =
    // TODO: Shadowing.
    val (varCtx, filteredCtx) = clauses.elems.partition(_ match
      case bound : Bound if bound.name == varName =>
        true
      case _ =>
        false
    )

    val varBounds = varCtx.flatMap(_ match
      case bound : Bound =>
        Some(bound)
      case _ =>
        None
    )

    val (lowerBounds, upperBounds) = varBounds.partition(_.dir match
      case Direction.Sub =>
        true
      case Direction.Super =>
        false
    )

    given Clauses = clauses
    val lowerBound = meetMany(lowerBounds.map(_.type_))
    val upperBound = joinMany(upperBounds.map(_.type_))

    return (filteredCtx, (lowerBound, upperBound))

  /** Retrain the bounds unsatisfied in the context. */
  def filterUnsatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter((bound) => !clauses.checkBoundSatisfied(bound))

  def getVarBounds(var_ : TypeVar): (List[Type], List[Type]) =
    val lowerBounds: ListBuffer[Type] = new ListBuffer()
    val upperBounds: ListBuffer[Type] = new ListBuffer()

    for bound <- clauses.varBounds(var_.name) do
      bound.dir match
        case Direction.Sub =>
          upperBounds.append(bound.type_)
        case Direction.Super =>
          lowerBounds.append(bound.type_)

    (lowerBounds.toList, upperBounds.toList)


/** Remove the variables in the context that appear before a certain level. */
def removeLowVars(ctx: Clauses, vars: List[TypeVar]): List[TypeVar] =
  // TODO
  vars
