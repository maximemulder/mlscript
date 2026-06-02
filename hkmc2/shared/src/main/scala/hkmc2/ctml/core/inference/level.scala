package hkmc2.ctml.core.inference

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.core.type_.impls.inline.*
import hkmc2.ctml.core.type_.impls.simplify.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withInferenceLevel(f: (TypeVar, Context) => (Type, Clauses)): (Type, Clauses) =
    val decl = ctx.declInferVar()
    ctx.withFreshVarLevel(TypeVarKind.Flex, List(decl), (a, b) => f(a(0), b), (a, b, c) => ctx.processLevel(a, b, c))

  /** Process the type, variables, and constraints generated in a level. Quantifying and
   *  simplifying then if possible. */
  def processLevel(level: Int, type_ : Type, outs: Clauses): (Type, Clauses) =
    val (type1, outs1) = type_.hoistCtx.unwrapCtx(using ctx.extend(outs))

    val (type2, outs2) = simplifyLevel(level, type1, outs.concat(outs1))

    val levelVars = ctx.extend(outs2).getLevelVars(level)
    val actions = levelVars.map((var_) => var_ -> determineVarAction(level, type2, var_, outs2)).toMap
    val varsToQuantify = getQuantifyVars(actions)

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type2, outs2)(using ctx)

    val (type3, outs3) = quantifyLevelBounds(makePrettyType(type2), level, outs2)(using ctx)

    val (type4, outs4) = varsToQuantify.foldRight((type3, outs3))((var_, to) =>
      quantifyVar(to._1, var_, to._2)(using ctx)
    )

    (type4, outs4)

  /** Iteratively collect and simplify the variables in a level until no further simplification is
   *  possible. */
  def simplifyLevel(level: Int, type_ : Type, outs: Clauses): (Type, Clauses) =
    val levelBounds = ctx.extend(outs).getLevelBounds(level)
    val levelVars = ctx.extend(outs).getLevelVars(level)
    val actions = levelVars.map((var_) => var_ -> determineVarAction(level, type_, var_, outs)).toMap

    getInlineVars(actions) match
      case Nil =>
        (type_, outs)
      case varsToInline =>
        val (nextType, nextOuts) = inlineVars(type_, outs, varsToInline)
        simplifyLevel(level, nextType, nextOuts)

  /** Determine how to process a variable of this level. */
  def determineVarAction(level: Int, type_ : Type, var_ : TypeVar, outs: Clauses): VarAction =
    given Context = ctx.extend(outs)
    val polarities = type_.getAllVarPolarities(var_)
    if ctx.extend(outs).getTypeVarEffectiveLevel(var_) < level then
      return debugVarAction(var_, type_, VarAction.Quantify, "bound at lower level")

    if polarities == Polarities(false, false) then
      return debugVarAction(var_, type_, VarAction.Inline, s"polarities ${polarities}")

    if polarities == Polarities(false, true) then
      if var_.isIndirectRecursive(Polarity.Positive) then
        return debugVarAction(var_, type_, VarAction.Quantify, "recursive")

      return debugVarAction(var_, type_, VarAction.Inline, s"polarities ${polarities}")

    if polarities == Polarities(true, false) then
      if var_.isIndirectRecursive(Polarity.Negative) then
        return debugVarAction(var_, type_, VarAction.Quantify, "recursive")

      return debugVarAction(var_, type_, VarAction.Inline, s"polarities ${polarities}")

    if checkEqual(var_.lowerBound, var_.upperBound) then
      return debugVarAction(var_, type_, VarAction.Inline, s"sandwich ${var_.lowerBound} ${var_.upperBound}")

    debugVarAction(var_, type_, VarAction.Quantify, "default")

  /** Inline a list of type variables. */
  def inlineVars(type_ : Type, outs: Clauses, vars: List[TypeVar]): (Type, Clauses) =
    vars.foldLeft((type_, outs))((to, var_) =>
      inlineVar(to._1, var_, to._2)(using ctx.extend(to._2))
    )

/** Get the type variables to inline in a mapping of type variable actions. */
def getInlineVars(actions: Map[TypeVar, VarAction]): List[TypeVar] =
  actions.filter((_, action) => action == VarAction.Inline).keys.toList

/** Get the type variables to quantify in a mapping of type variable actions. */
def getQuantifyVars(actions: Map[TypeVar, VarAction]): List[TypeVar] =
  actions.filter((_, action) => action == VarAction.Quantify).keys.toList

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugInlineVar(inlineVarImpl)(type_, var_, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    type_.inline(var_),
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
  )

def quantifyLevelBounds(type_ : Type, level: Int, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  val levelBounds = ctx.extend(outs).getLevelBounds(level)
  val newOuts = outs.filterBounds(_.highLevel(using ctx.extend(outs)) < level)
  (
    makeConstrainedType(type_, levelBounds.map(_.toConstraint)),
    newOuts
  )

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, outs)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    TUniv(var_, type_),
    outs.removeTypeVar(var_),
  )

/** Check whether a type contains outer unsolvable constrained types. */
def checkUnsolvableConstreds(type_ : Type, outs: Clauses)(using ctx: Context) =
  val (_, constraints) = type_.getConstrainedComponents
  var clauses = outs
  for constraint <- constraints do
    clauses = subtypeConstraintSeq(constraint, clauses)(using ctx, ConstraintMode.Solve)
