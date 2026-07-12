package hkmc2.ctml.core.inference

import hkmc2.ctml.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.*
import hkmc2.ctml.core.type_.impls.inline.*
import hkmc2.ctml.core.simplification.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Evaluate a type inference function in a new level with a new fresh type variable and solve
   *  that level. */
  def withInferenceLevel2(f: (TypeVar, SubContext) => (Type, SubClauses)): (Type, SubClauses) =
    val decl = ctx.declInferVar()
    ctx.withFreshVarLevel(TypeVarKind.Flex, List(decl), (a, b) => f(a(0), b), (a, b, c) => ctx.processLevel(a, b, c))

  def withInferLevel(f: (SubContext) => (Type, SubClauses)): (Type, SubClauses) =
    ctx.withLevel((ctx) =>
      val level = ctx.level
      val (type_, outs) = f(ctx)
      ctx.processLevel(level, type_, outs)
    )

  /** Process the type, variables, and constraints generated in a level. Quantifying and
   *  simplifying then if possible. */
  def processLevel(level: Int, type_ : Type, outs: SubClauses): (Type, SubClauses) =
    val (type1, outs1) = type_.hoistCtx.unwrapCtx(using ctx.extend(outs))

    // val (type2, outs2) = ctx.simplifyClauses(type1, level, outs.concat(outs1))
    val (type2, outs2) = ctx.simplifyLevel(type1, level, outs.concat(outs1))

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type2, outs2)(using ctx)

    val levelCtx = ctx.extend(outs2)
    val levelVars = levelCtx.getLevelVars(level)
    val noSimplifyInlineVars = levelVars
      .filter(levelCtx.getTypeVarEffectiveLevel(_) < level)
      .toSet

    val type5 = type2.wrapCtx(outs2)

    // val type5 = type4.simplify()(using ctx, NoInlineVars(noSimplifyInlineVars))

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type5, SubClauses.empty)(using ctx)

    (type5, SubClauses())

extension (ctx: TypeContext)
  /** Evaluate a type inference function in a new subtyping level and solve that level. */
  def withInferLevel(f: (TypeContext) => (Type, SubClauses)): (Type, SubClauses) =
    ctx.sub.withLevel((subCtx) =>
      val level = subCtx.level
      val innerCtx = ctx.copy(sub = subCtx)
      val (type_, outs) = f(innerCtx)
      subCtx.processLevel(level, type_, outs)
    )

/** Inline a type variable in a type. */
def inlineVar(type_ : Type, var_ : TypeVar, polarities: Polarities, outs: SubClauses)(using ctx: SubContext) =
  debugInlineVar(inlineVarImpl)(type_, var_, polarities, outs)

/** Implementation of `inlineVar`. */
def inlineVarImpl(type_ : Type, var_ : TypeVar, polarities: Polarities, outs: SubClauses)(using ctx: SubContext) =
  val relevantOuts = outs.filterBounds((bound) =>
    bound.var_ != var_ || polarities.contains(bound.dir.leftPol)
  )
  given SubContext = ctx.extend(relevantOuts)
  (
    type_.inline(var_),
    outs.mapBounds(_.inline(var_)).removeTypeVar(var_),
  )

/** Check whether a type contains outer unsolvable constrained types. */
def checkUnsolvableConstreds(type_ : Type, outs: SubClauses)(using ctx: SubContext) =
  val (_, constraints) = type_.getConstrainedComponents
  var clauses = outs
  for constraint <- constraints do
    clauses = subtypeConstraintSeq(constraint, clauses)(using ctx, ConstraintMode.Solve)
