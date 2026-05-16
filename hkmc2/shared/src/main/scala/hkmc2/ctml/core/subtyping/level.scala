package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a subtyping function in a new level with a new fresh type variable and solve that
   *  level. */
  def withSubtypingLevel(kind: TypeVarKind, original: TypeVar, f: (TypeVar, Context) => Clauses): Clauses =
    ctx.withFreshVarLevel(kind, Some(original), (a, b) => ((), f(a, b)), (_, _, b) => ((), b))._2
