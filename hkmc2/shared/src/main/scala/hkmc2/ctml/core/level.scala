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
      ctx.processLevelVar(te._1, var_, levelVars.toSet, te._2)
    )

  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withFreshVarLevel(f: (TypeVar, Context) => (Type, Clauses)): (Type, Clauses) =
    // Create a new fresh type variable, make it a type, and add it to the context.
    val freshDecl = newInferFreshVar()
    val freshCtx = ctx.extend(freshDecl)

    // Evaluate the type inference function with the fresh type variable.
    val (type_ , typeOuts) = f(freshDecl.var_, freshCtx)

    // Count the fresh type variable as belonging to this level.
    val outs =
      given Context = freshCtx
      freshDecl.asClauses.concat(typeOuts)

    // Solve the level.
    ctx.solveLevel(type_, outs)

  def processLevelVar(type_ : Type, var_ : TypeVar, levelVars: Set[TypeVar], outs: Clauses): (Type, Clauses) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx
    val lowerBound = fullCtx.getVarLowerBound(var_)
    val upperBound = fullCtx.getVarUpperBound(var_)
    val polarities = type_.getVarPolarities(var_)(using Polarity.Positive)
    val newType = if fullCtx.isVarConstrained(var_, levelVars) || polarities == Polarities(true, true) then
      quantifyVar(type_, var_, lowerBound, upperBound)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_, upperBound)
    else if polarities == Polarities(false, true) then
      inlineVar(type_, var_, lowerBound)
    else
      ignoreVar(type_, var_)
    // TODO: Remove variable
    (newType, outs.removeTypeVar(var_))

  /** Get the type variables of this level. */
  def getLevelVars(outs: Clauses): List[TypeVar] =
    // Get the type variables declared at this level.
    val vars = outs.typeVarDecls.map(_.var_).toList

    // Remove the variables that have dependent variabled declared at a lower level.
    vars.filter(var_ =>
      // Get the list of type variables that depend on the type variable of this level.
      val dependentVars = outs.getDependentVars(var_)

      // Check whether any of the dependent variables is declared at a lower level.
      !dependentVars.exists(Clauses(ctx.clauses).hasVar(_))
    )

  /** Check whether a type variable is constrained by any of the other variables of the same level. */
  def isVarConstrained(var_ : TypeVar, levelVars: Set[TypeVar]): Boolean =
    Direction.both.exists(
      ctx.getVarBound(var_, _).getConstrainedVars().contains(var_),
    )

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type)(using ctx: Context): Type =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, lowerBound, upperBound)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type)(using ctx: Context): Type =
  attachConstrainedBounds(type_, var_, lowerBound, upperBound)

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, bound: Type)(using ctx: Context): Type =
  debugInlineVar(inlineVarImpl)(type_, var_, bound)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, bound: Type)(using ctx: Context): Type =
  type_.substitute(var_, bound)

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, var_ : TypeVar)(using ctx: Context): Type =
  debugIgnoreVar(ignoreVarImpl)(type_, var_)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, var_ : TypeVar)(using ctx: Context): Type =
  type_
