package hkmc2.ctml.core.context

import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a function in a new level with a new fresh type variable and solve that level. */
  def withFreshVarLevel[T](
    kind: TypeVarKind,
    original: Option[TypeVar],
    inner: (TypeVar, Context) => (T, Clauses),
    outer: (T, Clauses) => (T, Clauses),
  ): (T, Clauses) =
    // Create a new fresh type variable and add it to the context.
    val (freshVar, freshCtx) = ctx.declFreshVar(kind, original)

    // Evaluate the inner function with the type variable in the context.
    val (res, innerOuts) = inner(freshVar, freshCtx)

    val freshDecl = freshCtx.clauses.find(_ match
      case TypeVarDecl(var_, _, _, _) =>
        true
      case _ =>
        false
    ).get

    // Move the type variable to the output clauses.
    val outs = Clauses.single(freshDecl).concat(innerOuts)

    // Evaluate the outer function with the type variable in the output clauses.
    outer(res, outs)
