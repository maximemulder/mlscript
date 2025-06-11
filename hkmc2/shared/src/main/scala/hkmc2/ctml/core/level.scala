package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.types.*

extension (ctx: Clauses)
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
  def withFreshVarLevel(f: (TVar, Clauses) => (Type, Clauses)): (Type, Clauses) =
    // Create a new fresh type variable, make it a type, and add it to the context.
    val freshVar = newInferFreshVar()
    val freshCtx = ctx.addClause(freshVar)
    val freshType = TVar(freshVar.name)

    // Evaluate the type inference function with the fresh type variable.
    val (type_ , typeOuts) = f(freshType, freshCtx)

    // Count the fresh type variable as belonging to this level.
    val outs = freshVar.asClauses.addClauses(typeOuts)

    // Solve the level.
    ctx.solveLevel(type_, outs)

  def processLevelVar(type_ : Type, var_ : TypeVar, levelVars: Set[String], outs: Clauses): (Type, Clauses) =
    val fullCtx = ctx.addClauses(outs)
    given Clauses = fullCtx
    val lowerBound = fullCtx.getVarLowerBound(var_.name)
    val upperBound = fullCtx.getVarUpperBound(var_.name)
    val polarities = type_.getVarPolarities(var_.name)(using Polarity.Positive)
    val newType = if fullCtx.isVarConstrained(var_.name, levelVars) then
      attachConstrainedBounds(type_, var_.name, lowerBound, upperBound)
    else
      polarities match
        case Polarities(true, true) =>
          attachConstrainedBounds(type_, var_.name, lowerBound, upperBound)
        case Polarities(false, false) =>
          type_
        case Polarities(true, false) =>
          type_.substitute(var_.name, upperBound)
        case Polarities(false, true)  =>
          type_.substitute(var_.name, lowerBound)
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
      !dependentVars.exists(ctx.hasVar(_))
    )

  /** Check whether a type variable is constrained by any of the other variables of the same level. */
  def isVarConstrained(varName: String, levelVars: Set[String]): Boolean =
    val types = levelVars.toList.flatMap(var_ => List.concat(
      ctx.getVarLowerBounds(var_),
      ctx.getVarUpperBounds(var_),
    ))

    types.exists(_.getConstrainedVars().contains(varName))
