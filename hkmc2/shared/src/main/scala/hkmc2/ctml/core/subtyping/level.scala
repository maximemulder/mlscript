package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a subtyping function in a new level with a new fresh type variable and solve that
   *  level. */
  def withSubtypingLevel(kind: TypeVarKind, originals: List[TypeVar], f: (List[TypeVar], Context) => Clauses): Clauses =
    val decls = ctx.declFreshVars(originals, kind)
    ctx.withFreshVarLevel(kind, decls, (a, b) => ((), f(a, b)), (_, _, b) => ((), b))._2

  def withSubtypingLevel2(f: (Int) => Clauses): Clauses =
    f(ctx.maxLevel + 1)
