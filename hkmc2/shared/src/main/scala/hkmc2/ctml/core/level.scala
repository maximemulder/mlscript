package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Solve a type inference level by processing each new variable of that level. */
  def solveLevel(type_ : Type, outs: Clauses): (Type, Clauses) =
    // Get the new type variables of this level.
    val levelVars = ctx.getLevelVars(outs)
    if levelVars == Nil then
      return (type_, outs)

    levelVars.foldRight((type_, outs))((var_, te) =>
      ctx.processLevelVar(te._1, var_, levelVars.map(_.name).toSet, te._2)
    )

  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withFreshVarLevel(f: (TVar, Context) => (Type, Clauses)): (Type, Clauses) =
    // Create a new fresh type variable, make it a type, and add it to the context.
    val freshVar = newInferFreshVar()
    val freshCtx = ctx.extend(freshVar)
    val freshType = TVar(freshVar.name)

    // Evaluate the type inference function with the fresh type variable.
    val (type_ , typeOuts) = f(freshType, freshCtx)

    // Count the fresh type variable as belonging to this level.
    val outs = freshVar.asClauses.concat(typeOuts)

    // Solve the level.
    ctx.solveLevel(type_, outs)

  def processLevelVar(type_ : Type, var_ : TypeVar, levelVars: Set[String], outs: Clauses): (Type, Clauses) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx
    val lowerBound = fullCtx.getVarLowerBound(var_.name)
    val upperBound = fullCtx.getVarUpperBound(var_.name)
    val polarities = type_.getVarPolarities(var_.name)(using Polarity.Positive)
    val newType = if fullCtx.isVarConstrained(var_.name, levelVars) || polarities == Polarities(true, true) then
      quantifyVar(type_, var_.name, lowerBound, upperBound)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_.name, upperBound)
    else if polarities == Polarities(false, true) then
      inlineVar(type_, var_.name, lowerBound)
    else
      ignoreVar(type_, var_.name)
    // TODO: Remove variable
    (newType, outs.removeTypeVar(var_.name))

  /** Get the type variables of this level. */
  def getLevelVars(outs: Clauses): List[TypeVar] =
    // Get the type variables declared at this level.
    val vars = outs.typeVars

    // Remove the variables that have dependent variabled declared at a lower level.
    vars.filter(var_ =>
      // Get the list of type variables that depend on the type variable of this level.
      val dependentVars = outs.getDependentVars(var_.name)

      // Check whether any of the dependent variables is declared at a lower level.
      !dependentVars.exists(Clauses(ctx.clauses).hasVar(_))
    )

  /** Check whether a type variable is constrained by any of the other variables of the same level. */
  def isVarConstrained(name: String, levelVars: Set[String]): Boolean =
    val types = levelVars.toList.flatMap(var_ => List.concat(
      ctx.getVarLowerBounds(var_),
      ctx.getVarUpperBounds(var_),
    ))

    types.exists(_.getConstrainedVars().contains(name))

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, name: String, lowerBound: Type, upperBound: Type)(using ctx: Context): Type =
  debugQuantifyVar(quantifyVarImpl)(type_, name, lowerBound, upperBound)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, name: String, lowerBound: Type, upperBound: Type)(using ctx: Context): Type =
  attachConstrainedBounds(type_, name, lowerBound, upperBound)

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, name: String, bound: Type)(using ctx: Context): Type =
  debugInlineVar(inlineVarImpl)(type_, name, bound)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, name: String, bound: Type)(using ctx: Context): Type =
  type_.substitute(name, bound)

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, name: String)(using ctx: Context): Type =
  debugIgnoreVar(ignoreVarImpl)(type_, name)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, name: String)(using ctx: Context): Type =
  type_
