package hkmc2.ctml.core.context

import hkmc2.ctml.types.Context
import hkmc2.ctml.types.Clause

extension (ctx: Context)
  /** Map over the clauses of the context as a single iterator. */
  def map(f: Iterator[Clause] => Iterator[Clause]): Context =
    Context(f(ctx.clauses.iterator).toList)

  /** Map over the clauses of the context. */
  def mapClauses(f: Clause => Clause): Context =
    ctx.map(_.map(f))
