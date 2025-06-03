package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.ctml.core.traverse.*

extension (ctx: Clauses)
  /** Solve a type inference level by processing each new variable of that level. */
  def solveLevel(type_ : Type, outs: Clauses): (Type, Clauses) =
    // TODO: Repeat while new variables to process ?

    // Get the new type variables present in the generated constraints.
    val newVars = outs.typeVars.toList
    // Remove the variables that appear in lower polymorphism levels.
    val filteredVars = ctx.filterLevelVars(newVars.toList, outs)

    // TODO:
    // 1. Find variables declared in statements.
    // 2. Remove variables that are indirectly referenced in ctx
    // 3. For each variable
    //     Get polarities.
    //     Extract variable declarion and bounds from statements (???)
    //     Substitute in context.
    //     Substitute in type.
    filteredVars.foldRight((type_, outs))((var_, te) =>
      ctx.processLevelVar(te._1, var_, filteredVars.map(_.name).toSet, te._2)
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
    val polarities =
      given Polarity = Polarity.Positive
      getTypePolarities(type_, var_.name)
    val newType = if fullCtx.isVarConstrained(var_.name, levelVars) then
      attachConstrainedBounds(type_, var_.name, lowerBound, upperBound)
    else
      polarities match
        case Polarities(true, true) =>
          attachConstrainedBounds(type_, var_.name, lowerBound, upperBound)
        case Polarities(false, false) =>
          type_
        case Polarities(true, false) =>
          substitute(type_, var_.name, upperBound)
        case Polarities(false, true)  =>
          substitute(type_, var_.name, lowerBound)
    // TODO: Remove variable
    (newType, outs.removeTypeVar(var_.name))

  /** Remove the variables in the context that appear before a certain level. */
  def filterLevelVars(vars: List[TypeVar], outs: Clauses): List[TypeVar] =
    val fullCtx = ctx.addClauses(outs)
    vars.filter(var_ =>
      val dependentVars = fullCtx.getDependentVars(var_.name)
      !dependentVars.exists(dependentVar => ctx.hasVar(dependentVar))
    )

  /** Check whether a type variable is constrained by any of the other variables of the same level. */
  def isVarConstrained(varName: String, levelVars: Set[String]): Boolean =
    val types = levelVars.toList.flatMap(var_ => List.concat(
      ctx.getVarLowerBounds(var_),
      ctx.getVarUpperBounds(var_),
    ))

    types.exists(_.getConstrainedVars().contains(varName))
