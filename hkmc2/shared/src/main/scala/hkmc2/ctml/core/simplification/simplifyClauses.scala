package hkmc2.ctml.core.simplification

import scala.annotation.tailrec

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Simplify local variables in an open inference result, including relations recorded only on
   *  another variable. Keep variables whose effective level belongs to an enclosing scope. */
  @tailrec
  def simplifyClauses(type_ : Type, level: Int, outs: SubClauses)(using noInlineVars: NoInlineVars): (Type, SubClauses) =
    val fullCtx = ctx.extend(outs)
    val vars = outs.typeVars
      .filterNot(noInlineVars.contains)
      .filter(fullCtx.getTypeVarEffectiveLevel(_) >= level)
    // Prefer ordinary inlining across all candidates before trying to recover reversed bounds.
    // Search from the oldest declaration, and restart after each removal: dependencies and
    // effective levels may change. Every successful step removes one declaration.
    val inlining = vars.reverseIterator.flatMap(tryInlineVar(type_, _, outs)(using ctx)).nextOption()
    val simplification = inlining.orElse(
      vars.reverseIterator.flatMap(tryEliminateVar(type_, _, outs)(using ctx)).nextOption()
    )
    simplification match
      case Some((newType, newOuts)) =>
        ctx.simplifyClauses(newType, level, newOuts)
      case None =>
        (type_, outs)
