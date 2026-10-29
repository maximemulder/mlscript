package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.context.extend
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Eliminate local variables before simplifying the result type under the remaining clauses.
   *  This leaves the level open: its caller is responsible for solving and quantifying it. */
  def simplifyLevel(type_ : Type, level: Int, outs: SubClauses)(using noInlineVars: NoInlineVars): (Type, SubClauses) =
    val (newType, newOuts) = ctx.simplifyClauses(type_, level, outs)
    (newType.simplify()(using ctx.extend(newOuts), noInlineVars), newOuts)
