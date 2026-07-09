package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Evaluate a subtyping function in a new level with a new fresh type variable and solve that
   *  level. */
  def withSubtypingLevel3(kind: TypeVarKind, originals: List[TypeVar], f: (List[TypeVar], SubContext) => SubClauses): SubClauses =
    val decls = ctx.declFreshVars(originals, kind)
    ctx.withFreshVarLevel(kind, decls, (a, b) => ((), f(a, b)), (_, _, b) => ((), b))._2

  def withSubtypingLevel2(f: (Int) => SubClauses): SubClauses =
    f(ctx.maxLevel + 1)

  def withSubtypingLevel(f: (SubContext) => SubClauses): SubClauses =
    ctx.withLevel((ctx) => ((), f(ctx)))._2

  def withFreshVars(kind: TypeVarKind, originals: List[TypeVar], f: (List[TypeVar], SubContext) => SubClauses): SubClauses =
    val decls = ctx.declFreshVars(originals, kind)
    f(decls.map(_.var_), ctx.extend(decls))
