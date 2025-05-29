package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.ctml.core.traverse.*

extension (ctx: Clauses)
  /** Evaluate a function within a context with a new fresh type variable. */
  def withLevel(f: Clauses => (Type, Clauses)): (Type, Clauses) =
    // Evluate the inference function and get the generated type and constraints.
    val (type_ , outs) = f(ctx)
    val typeCtx = ctx.addClauses(outs)
    // Get the new type variables present in the generated constraints.
    val newVars = outs.typeVars
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
      processLevelVar(te._1, var_, te._2)
    )

  def processLevelVar(type_ : Type, var_ : TypeVar, outs: Clauses): (Type, Clauses) =
    val fullCtx = ctx.addClauses(outs)
    given Clauses = fullCtx
    val lowerBound = fullCtx.getVarLowerBound(var_.name)
    val upperBound = fullCtx.getVarUpperBound(var_.name)
    val polarities =
      given Polarity = Polarity.Positive
      getTypePolarities(type_, var_.name)
    val newType = polarities match
      case Polarities(false, false) =>
        type_
      case Polarities(true, false) if !upperBound.isConstraining() =>
        substitute(type_, var_.name, upperBound)
      case Polarities(false, true) if !lowerBound.isConstraining()  =>
        substitute(type_, var_.name, lowerBound)
      case Polarities(_, _) =>
        attachConstrainedBounds(type_, var_.name, lowerBound, upperBound)
    // TODO: Remove variable
    (newType, outs.removeTypeVar(var_.name))

  /** Remove the variables in the context that appear before a certain level. */
  def filterLevelVars(vars: List[TypeVar], outs: Clauses): List[TypeVar] =
    val fullCtx = ctx.addClauses(outs)
    vars.filter(var_ =>
      val dependentVars = fullCtx.getDependentVars(var_.name)
      !dependentVars.exists(dependentVar => ctx.hasVar(dependentVar))
    )
