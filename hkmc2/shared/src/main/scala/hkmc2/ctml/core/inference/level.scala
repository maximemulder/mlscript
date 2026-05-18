package hkmc2.ctml.core.inference

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.getExtremalType
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.core.type_.impls.inline.*
import hkmc2.ctml.core.type_.impls.simplify.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.OrderedSet as MutSet

extension (ctx: Context)
  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withInferenceLevel(f: (TypeVar, Context) => (Type, Clauses)): (Type, Clauses) =
    ctx.withFreshVarLevel(TypeVarKind.Flex, None, f, (a, b, c) => ctx.solveLevel(a, b, c))

  def solveLevel(level: Int, type_ : Type, outs: Clauses, quantifyVars: List[TypeVar] = List()): (Type, Clauses) =
    val levelVars = ctx.extend(outs).getLevelVars(level)
    var quantifyVarsMut = List[TypeVar]()
    val (newType, newOuts) = levelVars.foldLeft((type_, outs))((x, var_) =>
      val (newType, newOuts, quantify) = ctx.processLevelVar(x._1, var_, x._2)
      if quantify then
        quantifyVarsMut = var_ :: quantifyVarsMut

      (newType, newOuts)
    )

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(newType, newOuts)(using ctx)

    quantifyVarsMut.foldRight((newType, newOuts))((var_, to) =>
      quantifyVar2(to._1, var_, to._2)(using ctx)
    )

  def processLevelVar(type_ : Type, var_ : TypeVar, outs: Clauses) =
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
        quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, false) then
      inlineVar(type_, var_, outs)
    else
      inlineVar(type_, var_, outs)

/** Quantify a type variable in a type. */
def quantifyVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugQuantifyVar(quantifyVarImpl)(type_, var_, outs)

/** Implementation of `quantifyVar`. */
def quantifyVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
    (type_, outs, true)

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugInlineVar(inlineVarImpl)(type_, var_, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    type_.inline(var_),
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
    false,
  )

/** Ignore a type variable in a type. */
def ignoreVar(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  debugIgnoreVar(ignoreVarImpl)(type_, var_, outs)

/** Implementation of `ignoreVar`. */
def ignoreVarImpl(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context) =
  (
    type_,
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
    false,
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

/** Check whether a type contains outer unsolvable constrained types. */
def checkUnsolvableConstreds(type_ : Type, outs: Clauses)(using ctx: Context) =
  val (_, constraints) = type_.getConstrainedComponents
  var clauses = outs
  for constraint <- constraints do
    clauses = subtypeConstraintSeq(constraint, clauses)(using ctx, ConstraintMode.Solve, SubtypingCache())
