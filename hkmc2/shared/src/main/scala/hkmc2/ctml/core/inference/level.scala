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
  /** Process the type, variables, and constraints generated in a level. Quantifying and
   *  simplifying then if possible. */
  def processLevel(level: Int, type_ : Type, outs: SubClauses): Type =
    // NOTE: unwrapCtx currently solves clauses, which may need to be moved somewhere else.

    debug(s"LEVEL CLAUSES (${level}) ${type_} OUT ${outs}")

    val (type1, typeOuts0) = type_.hoistCtx.unwrapCtx

    val typeOuts1 = solve(typeOuts0)(using ctx.extend(outs))

    debug(s"LEVEL UNWRAP ${type1} OUT ${typeOuts1}")

    // Variables extruded through an outer variable have an effective level below their declaration
    // level. Keep them quantified: eliminating them by polarity would lose that outer dependency.
    val levelCtx = ctx.extend(outs.concat(typeOuts1))
    val noInlineVars = NoInlineVars(
      levelCtx.getLevelVars(level)
        .filter(levelCtx.getTypeVarEffectiveLevel(_) < level)
        .toSet
    )

    val (type2, typeOuts2) = ctx.extend(outs).simplifyLevel(type1, level, typeOuts1)(using noInlineVars)

    debug(s"LEVEL SIMPLIFY TYPE ${type2} OUT ${typeOuts2}")

    val type3 = type2.wrapCtx(typeOuts2)

    debug(s"LEVEL WRAP TYPE ${type3}")

    val (type4, outs4) = ctx.simplifyLevel(type3, level, outs)(using noInlineVars)

    debug(s"LEVEL SIMPLIFY ${type4} OUT ${outs4}")

    if config.checkUnsolvableConstreds then
      checkUnsolvableConstreds(type4, outs4)(using ctx)

    val type5 = type4.wrapCtx(outs4)

    debug(s"LEVEL RESULT ${type5}")

    type5

extension (ctx: TypeContext)
  /** Evaluate a type inference function in a new subtyping level and solve that level. */
  def withInferLevel(f: (TypeContext) => (Type, SubClauses)): Type =
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
