package hkmc2.ctml.core

import hkmc2.ctml.util.OrderedSet as MutSet

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.core.combine.getExtremalType
import hkmc2.ctml.core.type_.traits.removeVarDirectCycles
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.core.type_.impls.inline.*

extension (ctx: Context)
  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withFreshVarLevel(f: (TypeVar, Context) => (Type, Clauses)): (Type, Clauses) =
    // Create a new fresh type variable, make it a type, and add it to the context.
    val freshDecl = declFreshFlexVar()
    val freshCtx = ctx.extend(freshDecl)

    // Evaluate the type inference function with the fresh type variable.
    val (type_ , typeOuts) = f(freshDecl.var_, freshCtx)

    // Count the fresh type variable as belonging to this level.
    val outs =
      given Context = freshCtx
      freshDecl.asClauses.concat(typeOuts)

    // Solve the level.
    ctx.solveLevel(type_, outs)

  /** Get the type variables of this level. */
  def getLevelVars(outs: Clauses): List[TypeVar] =
    // Get the variable dependency graph of each type variable declared at this level.
    val dependencies = outs.typeVars.map(_.getDependencies()(using ctx.extend(outs)))

    // Sort the variables such that each variable appears before its dependent variables.
    val vars = dependencies.getSortedVars()

    // Return the variables that are not declared at lower levels.
    vars.filter(ctx.isLevelVar(_, outs))

  /** Check whether a type variable is a variable of this level, that is, if it not depended on by
   *  a variable of a lower level.
   */
  def isLevelVar(var_ : TypeVar, outs: Clauses)(using cache: MutSet[TypeVar] = MutSet()): Boolean =
    // Get the list of type variables that directly depend on the type variable.
    var dependentVars = outs.getDependentVars(var_)

    // Update the type variables and cache.
    dependentVars = dependentVars.filter(!cache.contains(_))
    cache.addAll(dependentVars)

    // The type variable is not declared in a lower level and neither are its dependent variables.
    !ctx.hasVar(var_) && dependentVars.forall(ctx.isLevelVar(_, outs))

  /** Check whether a type variable is constrained by any of the other variables of the same level. */
  def isVarConstrained(var_ : TypeVar, levelVars: Set[TypeVar]): Boolean =
    val types = levelVars.toList.flatMap(levelVar => List(
      ctx.getVarLowerBound(levelVar),
      ctx.getVarUpperBound(levelVar),
    ))

    types.exists(_.getConstrainedVars().contains(var_))

  def solveLevel(type_ : Type, touts: Clauses): (Type, Clauses) =
    val levelVars = ctx.getLevelVars(touts)
    // output(s"LEVEL VARS ${levelVars}")

    // TODO: Use full context or non-context function.
/*    val typeVars = type_.getVars()(using ctx.extend(outs))
    // output(s"TYPE VARS ${typeVars}")

    // Get the intersection of variables present in the type and in the level.
    val processVars = levelVars.filter(typeVars.contains(_))
    // output(s"COMMON VARS ${processVars}")

    // Get the variables that are present in the level but not in the type.
    val removeVars = levelVars.filter(!typeVars.contains(_))
    // output(s"REMOVE VARS ${removeVars}") */

    val outs = ctx.removeUnusedBounds(type_, touts, levelVars.toSet)

    // Ignore, inline, or add constraints for each type variables of this level.
    val (newType, newOuts) = levelVars.foldRight((type_, outs))((var_, to) =>
      ctx.processLevelVar(to._1, var_, to._2, levelVars.toSet)
    )

    // Get the type variables of this level that were not ignored or inlined.
    val remainingVars = levelVars.filter(hkmc2.ctml.core.clauses.hasVar(newOuts)(_))

    remainingVars.reverse.foldRight((newType, newOuts))((var_, to) =>
      quantifyVar2(to._1, var_, to._2)(using ctx)
    )

  def removeUnusedBounds(type_ : Type, outs: Clauses, levelVars: Set[TypeVar]): Clauses =
    outs.filterBounds(bound =>
      if !levelVars.contains(bound.var_) then
        true
      else
        val polarities = type_.getAllVarPolarities(bound.var_)(using ctx.extend(outs))
        bound.dir match
          case Direction.Sub =>
            polarities.negative
          case Direction.Super =>
            polarities.positive
    )

  def processLevelVar(type_ : Type, var_ : TypeVar, outs: Clauses, levelVars: Set[TypeVar]) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx
    val polarities = type_.getAllVarPolarities(var_)
    if polarities == Polarities(false, false) then
      ignoreVar(type_, var_, outs)
    else if var_.isRecursive then
      // output(s"RECURSIVE ${var_}")
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, true) then
      val lowerBound = fullCtx.getVarLowerBound(var_)
      val upperBound = fullCtx.getVarUpperBound(var_)
      // output(s"TRUE TRUE ${var_}")
      if checkEqual(lowerBound, upperBound) then
        inlineVar(type_, var_, outs)
      else
        quantifyVar(type_, var_, outs)
    else if fullCtx.isVarConstrained(var_, levelVars) then
      // output(s"CONSTRAINED ${var_}")
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_, outs)
    else
      inlineVar(type_, var_, outs)

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, outs)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  /* val bounds = removeImplicitBounds(List(
    Bound(var_, Direction.Super, lowerBound),
    Bound(var_, Direction.Sub, upperBound),
  ))

  (
    makeConstrainedType(type_, bounds),
    // Only the bounds of the variable are removed from the clauses, the variable declaration will
    // be removed later when all remaining type variables of this level are quantified.
    outs.removeTypeVarBounds(var_),
  ) */
    (type_, outs)

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugInlineVar(inlineVarImpl)(type_, var_, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  given Context = ctx.extend(outs)
  // TODO: Make that cleaner. Be careful, the simplifications of a bound must not rely on the bound
  // itself, as it is circular reasoning and can then lose valuable information.
  val b = outs.mapBounds(
    b => Bound(b.var_, b.dir, TypeInline1(b.type_, TypeInlineParams(var_, b.dir.pol, ctx, Some(b.var_))))
  )
  .removeTypeVar(var_)
  (
    type_.inline(var_),
    b,
  )

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugIgnoreVar(ignoreVarImpl)(type_, var_, outs)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  (
    type_,
    outs.mapBounds(b => Bound(b.var_, b.dir, TypeInline1(b.type_, TypeInlineParams(var_, b.dir.pol, ctx, Some(b.var_))))).removeTypeVar(var_),
  )

/** Quantify a type variable in a type. */
def quantifyVar2(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  // output(s"QUANTIFY VAR 2 ${var_}")
  // If a polarity is not reachable, remove the bound ???
  val fullCtx = ctx.extend(outs)
  val lowerBound = fullCtx.getVarLowerBound(var_)
  val upperBound = fullCtx.getVarUpperBound(var_)
  val bounds = removeImplicitBounds(List(
    Bound(var_, Direction.Super, lowerBound),
    Bound(var_, Direction.Sub, upperBound),
  ))

  (
    TUniv(var_, makeConstrainedType(type_, bounds)),
    outs.removeTypeVar(var_),
  )
