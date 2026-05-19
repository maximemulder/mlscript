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
    val (type2, outs2) = levelVars.foldLeft((type_, outs))((x, var_) =>
      val (newType, newOuts, quantify) = ctx.processLevelVar(level, x._1, var_, x._2)
      if quantify then
        quantifyVarsMut = var_ :: quantifyVarsMut

      (newType, newOuts)
    )

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type2, outs2)(using ctx)

    val (type3, outs3) = quantifyLevelBounds(makePrettyType(type2), level, outs2)(using ctx)

    val (type4, outs4) = quantifyVarsMut.foldRight((type3, outs3))((var_, to) =>
      quantifyVar2(to._1, var_, to._2)(using ctx)
    )

    (type4, outs4)


  def processLevelVar(level: Int, type_ : Type, var_ : TypeVar, outs: Clauses) =
    val fullCtx = ctx.extend(outs)
    given Context = fullCtx
    val polarities = type_.getAllVarPolarities(var_)
    if getTypeMinLevel(var_.lowerBound).exists(_ < level) || getTypeMinLevel(var_.upperBound).exists(_ < level) then
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(false, false) then
      ignoreVar(type_, var_, outs)
    else if var_.isRecursive  then
      quantifyVar(type_, var_, outs)
    else if polarities == Polarities(true, true) then
      if checkEqual(var_.lowerBound, var_.upperBound) then
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

def getTypeMinLevel(type_ : Type)(using ctx: Context): Option[Int] =
  Iterator
    .concat(type_.getVars())
    .map(_.level(using ctx))
    .minOption

def getBoundLevelMax(bound: Bound)(using ctx: Context): Int =
  Iterator
    .single(bound.var_)
    .concat(bound.type_.getVars())
    .map(_.level(using ctx))
    .max

def quantifyLevelBounds(type_ : Type, level: Int, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  val keep = outs.filterBounds(getBoundLevelMax(_)(using ctx.extend(outs)) < level)
  val quant = outs.bounds.filter(getBoundLevelMax(_)(using ctx.extend(outs)) >= level)

  (
    makeConstrainedType(type_, quant.map(_.toConstraint)),
    keep
  )

/** Quantify a type variable in a type. */
def quantifyVar2(type_ : Type, var_ : TypeVar, outs: Clauses)(using ctx: Context): (Type, Clauses) =
  (
    TUniv(var_, type_),
    outs.removeTypeVar(var_),
  )

/** Check whether a type contains outer unsolvable constrained types. */
def checkUnsolvableConstreds(type_ : Type, outs: Clauses)(using ctx: Context) =
  val (_, constraints) = type_.getConstrainedComponents
  var clauses = outs
  for constraint <- constraints do
    clauses = subtypeConstraintSeq(constraint, clauses)(using ctx, ConstraintMode.Solve, SubtypingCache())
