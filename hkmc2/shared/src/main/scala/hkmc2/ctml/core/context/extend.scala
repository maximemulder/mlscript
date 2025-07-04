package hkmc2.ctml.core.context

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.isConstraining

extension (ctx: Context)
  /** Extend the context with one or several clauses. */
  def extend(clauses: AsClauses*): Context =
    clauses
      .reverse
      .flatMap(_.asClauses)
      .foldRight(ctx)((clause, ctx) => ctx.extendOne(clause))

  /** Append a clause at the end of the clauses. */
  def extendOne(clause: Clause): Context =
    Context(clause :: ctx.clauses)
