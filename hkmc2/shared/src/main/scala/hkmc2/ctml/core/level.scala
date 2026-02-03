package hkmc2.ctml.core

import hkmc2.ctml.util.OrderedSet as MutSet

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.combine.getExtremalType
import hkmc2.ctml.core.system.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.core.type_.impls.inline.*
import hkmc2.ctml.core.type_.impls.simplify.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

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

    val outs = ctx.removeUnusedBounds(type_, touts, levelVars.toSet)

    // Ignore, inline, or add constraints for each type variables of this level.
    val (newType, newOuts, rerun) = levelVars.foldRight((type_, outs, false))((var_, to) =>
      ctx.processLevelVar(to._1, var_, to._2, levelVars.toSet)
    )

    if rerun then
      return ctx.solveLevel(newType, newOuts)

    // Get the type variables of this level that were not ignored or inlined.
    val remainingVars = levelVars.filter(hkmc2.ctml.core.clauses.hasVar(newOuts)(_))

    val (newNewType, newNewOuts) = remainingVars.reverse.foldRight((newType, newOuts))((var_, to) =>
      quantifyVar2(to._1, var_, to._2)(using ctx)
    )

    (newNewType.simplify()(using ctx.extend(newNewOuts)), newNewOuts)

  def removeUnusedBounds(type_ : Type, outs: Clauses, levelVars: Set[TypeVar]): Clauses =
    outs.filterBounds(bound =>
      if !levelVars.contains(bound.var_) then
        true
      else
        val polarities = type_.getAllVarPolarities(bound.var_)(using ctx.extend(outs))
        polarities.contains(bound.dir.pol)
    )

  def processLevelVar(type_ : Type, var_ : TypeVar, outs: Clauses, levelVars: Set[TypeVar]) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx
    val polarities = type_.getAllVarPolarities(var_)
    if polarities == Polarities(false, false) then
      ignoreVar(type_, var_, outs)
    else if var_.isRecursive then
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, true) then
      val lowerBound = fullCtx.getVarLowerBound(var_)
      val upperBound = fullCtx.getVarUpperBound(var_)
      if checkEqual(lowerBound, upperBound) then
        inlineVar(type_, var_, outs)
      else
        // type_.getAllProperVarPolarities(var_) match
        //   case Polarities(true, false) | Polarities(false, true) =>
        //     output(s"SIMPLIFY ${var_}")
        //   case _ =>
        //     ()
        quantifyVar(type_, var_, outs)
    else if fullCtx.isVarConstrained(var_, levelVars) then
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_, outs)
    else
      inlineVar(type_, var_, outs)
    // val fullCtx = ctx.extend(outs)
    // given Context = fullCtx
    // quantifyVar(type_, var_, outs)

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, outs)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
    (type_, outs, false)

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugInlineVar(inlineVarImpl)(type_, var_, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    type_.inline(var_),
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
    true,
  )

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugIgnoreVar(ignoreVarImpl)(type_, var_, outs)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    type_,
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
    true,
  )

/** Quantify a type variable in a type. */
def quantifyVar2(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  // If a polarity is not reachable, remove the bound ???
  val fullCtx = ctx.extend(outs)
  val lowerBound = fullCtx.getVarLowerBound(var_)
  val upperBound = fullCtx.getVarUpperBound(var_)
  val bounds = removeImplicitBounds(List(
    Bound(var_, Direction.Super, lowerBound),
    Bound(var_, Direction.Sub, upperBound),
  )).map(_.toConstraint)

  (
    TUniv(var_, makeConstrainedType(type_, bounds)),
    outs.removeTypeVar(var_),
  )
