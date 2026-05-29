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
    ctx.withFreshVarLevel(TypeVarKind.Flex, List(decl), (a, b) => f(a(0), b), (a, b, c) => ctx.solveLevel(a, b, c))

  def solveLevel(level: Int, type_ : Type, outs: Clauses, quantifyVars: List[TypeVar] = List()): (Type, Clauses) =
    val levelBounds = ctx.extend(outs).getLevelBounds(level)
    val levelVars = ctx.extend(outs).getLevelVars(level)

    val actions = levelVars.map((var_) => var_ -> processVar(level, type_, var_, outs)).toMap
    val inlineVars = actions.filter((_, action) => action == VarAction.Inline).keys.toList
    val quantifyVars = actions.filter((_, action) => action == VarAction.Quantify).keys.toList

    val (type2, outs2) = inlineVars.foldLeft((type_, outs))((x, var_) =>
      inlineVar(x._1, var_, x._2)(using ctx.extend(x._2))
    )

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type2, outs2)(using ctx)

    val (type3, outs3) = quantifyLevelBounds(makePrettyType(type2), level, outs2)(using ctx)

    val (type4, outs4) = quantifyVars.foldRight((type3, outs3))((var_, to) =>
      quantifyVar(to._1, var_, to._2)(using ctx)
    )

    (type4, outs4)

  def processVar(level: Int, type_ : Type, var_ : TypeVar, outs: Clauses): VarAction =
    given Context = ctx.extend(outs)
    val polarities = type_.getAllVarPolarities(var_)
    if outs.bounds.exists((bound) => bound.var_.level < level && bound.type_.getVars.contains(var_)) then
      return debugVarAction(var_, VarAction.Quantify, "bound at lower level")

    if var_.isRecursive then
      return debugVarAction(var_, VarAction.Quantify, "recursive")

    if polarities != Polarities(true, true) then
      return debugVarAction(var_, VarAction.Inline, s"polarities ${polarities}")

    if checkEqual(var_.lowerBound, var_.upperBound) then
      return debugVarAction(var_, VarAction.Inline, s"sandwich ${var_.lowerBound} ${var_.upperBound}")

    debugVarAction(var_, VarAction.Quantify, "default")

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
