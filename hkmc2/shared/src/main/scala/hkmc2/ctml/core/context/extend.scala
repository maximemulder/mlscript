package hkmc2.ctml.core.context

import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.asClauses
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.debug.*
import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Extend the context with one or several clauses. */
  def extend(clauses: AsClauses2*): Context =
    clauses
      .reverse
      .flatMap(_.asClauses)
      .foldRight(ctx)((clause, ctx) => ctx.extendOne(clause))

  /** Append a clause at the end of the clauses. */
  def extendOne(clause: Clause): Context =
    clause match
      case boundi @ Bound(name, dir, type_) =>
        printDebug(s"ADD ${boundi}")
        val boundTypes = ctx.getVarBounds(name, dir)
        // TODO: Propagate constraining bounds.
        val boundType = (type_ :: boundTypes).combineMany(dir)(using ctx)
        val bound = Bound(name, dir, type_)
        Context(bound :: ctx.clauses)
      case clause =>
        Context(clause :: ctx.clauses)
