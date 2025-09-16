package hkmc2.ctml.core

import scala.collection.mutable.Set as SetMut
import scala.collection.mutable.ListBuffer

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (ctx: Context)
  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withFreshVarLevel(f: (TypeVar, Context) => (Type, Clauses)): (Type, Clauses) =
    // Create a new fresh type variable, make it a type, and add it to the context.
    val freshDecl = declNewFreshVar()
    val freshCtx = ctx.extend(freshDecl)

    // Evaluate the type inference function with the fresh type variable.
    val (type_ , typeOuts) = f(freshDecl.var_, freshCtx)

    // Count the fresh type variable as belonging to this level.
    val outs =
      given Context = freshCtx
      freshDecl.asClauses.concat(typeOuts)

    // Solve the level.
    ctx.solveLevel(type_, outs)

  /** Solve a type inference level by processing each new variable of that level. */
  def solveLevel(type_ : Type, outs: Clauses): (Type, Clauses) =
    // Get the type variables of this level.
    val levelVars = ctx.getLevelVars(outs)
    if levelVars == Nil then
      return (type_, outs)

    // Ignore, inline, or add constraints for each type variables of this level.
    val (newType, newOuts) = levelVars.foldRight((type_, outs))((var_, to) =>
      ctx.processLevelVar(to._1, var_, levelVars.toSet, to._2)
    )

    // Get the type variables of this level that were not ignored or inlined.
    val remainingVars = levelVars.filter(hkmc2.ctml.core.clauses.hasVar(newOuts)(_))

    remainingVars.reverse.foldRight((newType, newOuts))((var_, to) =>
      quantifyVar2(to._1, var_, to._2)
    )

  def processLevelVar(type_ : Type, var_ : TypeVar, levelVars: Set[TypeVar], outs: Clauses): (Type, Clauses) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx

    // Ignore type variables that are not directly or indirectly used by the type inferred.
    if !type_.usesVar(var_) then
      return ignoreVar(type_, var_, outs)

    val lowerBound = fullCtx.getVarLowerBound(var_)
    val upperBound = fullCtx.getVarUpperBound(var_)
    val polarities = type_.getVarPolarities(var_)
    if polarities == Polarities(true, true) || fullCtx.isVarConstrained(var_, levelVars) || fullCtx.isVarRecursive(var_) then
      quantifyVar(type_, var_, lowerBound, upperBound, outs)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_, upperBound, outs)
    else if polarities == Polarities(false, true) then
      inlineVar(type_, var_, lowerBound, outs)
    else
      ignoreVar(type_, var_, outs)

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
  def isLevelVar(var_ : TypeVar, outs: Clauses)(using cache: SetMut[TypeVar] = SetMut()): Boolean =
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

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, lowerBound, upperBound, outs)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  val bounds = removeImplicitBounds(List(
    Bound(var_, Direction.Super, lowerBound),
    Bound(var_, Direction.Sub, upperBound),
  ))

  (
    makeConstrainedType(type_, bounds),
    // Only the bounds of the variable are removed from the clauses, the variable declaration will
    // be removed later when all remaining type variables of this level are quantified.
    outs.removeTypeVarBounds(var_),
  )

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, bound: Type, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugInlineVar(inlineVarImpl)(type_, var_, bound, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, bound: Type, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  (
    type_.inline(var_, bound),
    outs.removeTypeVar(var_),
  )

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugIgnoreVar(ignoreVarImpl)(type_, var_, outs)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  (
    type_,
    outs.removeTypeVar(var_),
  )

/** Quantify a type variable in a type. */
def quantifyVar2(type_ : Type, var_ : TypeVar, outs: Clauses): (Type, Clauses) =
  (
    TUniv(var_, type_),
    outs.removeTypeVarDecl(var_),
  )
