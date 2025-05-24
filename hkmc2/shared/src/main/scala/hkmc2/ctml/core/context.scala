package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (ctx: Context)
  // Basic iterators

  /** Iterate over the term variables of the context. */
  def termVars: Iterator[TermVar] =
    ctx.entries.iterator.flatMap(_ match
      case var_ : TermVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the type variables of the context. */
  def typeVars: Iterator[TypeVar] =
    ctx.entries.iterator.flatMap(_ match
      case var_ : TypeVar =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the bounds of the context. */
  def bounds: Iterator[Bound] =
    ctx.entries.iterator.flatMap(_ match
      case bound : Bound =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the bounds of a type variable in the context. */
  def varBounds(varName: String): Iterator[Bound] =
    // TODO: Shadowing.
    ctx.bounds.filter((bound) => bound.name == varName)

  // Getters

  /** Get the type of a term variable in the context. */
  def getVarType(varName: String): Type =
    ctx.termVars.find(var_ => var_.name == varName) match
      case Some(var_) =>
        var_.type_
      case None =>
        throw new TypeError(s"Variable '${varName}' not found in the context.")

  /** Get a type variable in the context. */
  def getTypeVarKind(varName: String): TypeVarKind =
    ctx.typeVars.find((var_) => var_.name == varName) match
      case Some(var_) =>
        var_.kind
      case None =>
        throw new TypeError(s"Type variable '${varName}' not found in the context.")

  /** Setters */

  def addEntry(entry: ContextEntry*): Context =
    ctx.addEntries(entry.toList)

  def addEntries(entries: List[ContextEntry]*): Context =
    val newEntries = entries.reverse.flatten.toList
    Context(newEntries ::: ctx.entries)

  /** Join two lists of variables by removing duplicates. */
  def joinVars(lefts: List[String], rights: List[String]): List[String] =
    val filteredRights = rights.filter((right) => !(lefts.exists ((left) => left == right)))
    lefts ::: filteredRights

  // Merge bounds

  /** Merge two lists of bounds such that they must both be satisfied. */
  def meetBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    // Check if each right bound is satisfied in the left bounds to remove subsumed constraints.
    val filteredRights = ctx.addEntries(lefts).filterUnsatisfiedBounds(rights)
    // Be careful to check satisfaction against the *filtered* list of constraints to not remove duplicate
    // constraints entirely.
    val filteredLefts = ctx.addEntries(filteredRights).filterUnsatisfiedBounds(lefts)
    // Return the concatenation of the filtered bounds.
    filteredLefts ::: filteredRights

  /** Merge two lists of bounds such that either of those must be satisfied. */
  def joinBounds(lefts: List[Bound], rights: List[Bound]): List[Bound] =
    val lowerBounds = joinBoundsDir(lefts, rights, Direction.Sub)
    val upperBounds = joinBoundsDir(lefts, rights, Direction.Super)
    lowerBounds ::: upperBounds

  /** Join two lists of bounds in a given typing direction. */
  def joinBoundsDir(lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    val varNames = getBoundedVarsDir(lefts, rights, dir)
    ctx.joinVarsBoundsDir(varNames, lefts, rights, dir)

  /** Get the variables bounded in a given typing direction in either of two bound lists. */
  def getBoundedVarsDir(lefts: List[Bound], rights: List[Bound], dir: Direction): List[String] =
    val typeVars = ctx.typeVars.map(_.name).toList
    val leftVars  = lefts.filterBoundedVars(typeVars, dir)
    val rightVars = rights.filterBoundedVars(typeVars, dir)
    joinVars(leftVars, rightVars)

  /** Get the join bounds of some variables in two lists of constrains in a given typing direction. */
  def joinVarsBoundsDir(varNames: List[String], lefts: List[Bound], rights: List[Bound], dir: Direction): List[Bound] =
    varNames.map((varName) =>
      val type_ = ctx.joinVarBounds(varName, lefts, rights, dir)
      Bound(varName, dir, type_)
    )

  /** Get the join of the bounds of a variable in two lists of constraints. */
  def joinVarBounds(varName: String, lefts: List[Bound], rights: List[Bound], dir: Direction) =
    given Context = ctx
    val leftBounds  = lefts.collectVarBounds(varName, dir)
    val rightBounds = rights.collectVarBounds(varName, dir)
    val leftBound  = combineMany(leftBounds,  dir)
    val rightBound = combineMany(rightBounds, dir)
    val leftType  =
      given Context = ctx.addEntry(Bound(varName, dir, leftBound))
      attachConstrainingBounds(leftBound, lefts)
    val rightType =
      given Context = ctx.addEntry(Bound(varName, dir, rightBound))
      attachConstrainingBounds(rightBound, rights)
    join(leftType, rightType)

  // Combinators

  /** Evaluate all the given functions and meet their returned bounds. */
  def all(fs: (() => List[Bound])*): List[Bound] =
    fs.map(_()).flatten.toList

  /** Evaluate all the given functions and join their returned bounds. */
  def any(fs: (() => List[Bound])*): List[Bound] =
    val result = fs.foldRight(None: Option[List[Bound]])((f, result) =>
      try
        val bounds = f()
        result match
          case Some(resultBounds) =>
            Some(joinBounds(resultBounds, bounds))
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
        throw new TypeError("No alternative for any.")

  // Others

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(varName: String): Boolean =
    ctx.getTypeVarKind(varName) == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(varName: String): Boolean =
    ctx.getTypeVarKind(varName) == TypeVarKind.Rigid

  /** Get the lower bound of a type variable in the context. */
  def getVarLowerBound(varName: String): Type =
    ctx.varBounds(varName)
      .filter(_.dir == Direction.Super)
      .foldRight(TBot: Type)((bound, type_) =>
        given Context = ctx
        join(type_, bound.type_)
      )

  /** Get the upper bound of a type variable in the context. */
  def getVarUpperBound(varName: String): Type =
    ctx.varBounds(varName)
      .filter(_.dir == Direction.Sub)
      .foldRight(TTop: Type)((bound, type_) =>
        given Context = ctx
        meet(type_, bound.type_)
      )

  /** Extract a variable and its bounds from the context. */
  def extractVarBounds(varName: String): (List[ContextEntry], (Type, Type)) =
    // TODO: Shadowing.
    val (varCtx, filteredCtx) = ctx.entries.partition(_ match
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

    given Context = ctx
    val lowerBound = meetMany(lowerBounds.map(_.type_))
    val upperBound = joinMany(upperBounds.map(_.type_))

    return (filteredCtx, (lowerBound, upperBound))

  /** Retrain the bounds unsatisfied in the context. */
  def filterUnsatisfiedBounds(bounds: List[Bound]): List[Bound] =
    bounds.filter((bound) => !ctx.checkBoundSatisfied(bound))

def newFreshVar(): TypeVar =
  val varName = newFreshVarName()
  TypeVar(varName, TypeVarKind.Fresh)
