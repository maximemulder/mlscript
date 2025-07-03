package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

// Methods used to evaluate conjunctions and disjunctions within a typing context.

extension (ctx: Context)
  /** Evaluate some functions and meet the bounds returned. */
  def all(fs: (() => Context ?=> Clauses)*): Clauses =
    // A left fold preserves the arguments order if they are passed from left to right, which
    // should be the case if they are statically written inline, but should not the case if they.
    // come from a dynamically generated list.
    fs.foldLeft(Clauses.none)((clauses, f) => ctx.seq(f, clauses))

  /** Evaluate some functions and join the bounds returned. */
  def any(fs: (() => Context ?=> Clauses)*): Clauses =
    val errorTrees = ListBuffer[ProofTree]()

    // A left fold preserves the arguments order if they are passed from left to right, which
    // should be the case if they are statically written inline, but should not the case if they.
    // come from a dynamically generated list.
    val result = fs.foldLeft(None: Option[Clauses])((result, f) =>
      try
        given Context = ctx
        val bounds = f()
        result match
          case Some(resultBounds) =>
            Some(Clauses(ctx.joinBounds(resultBounds, bounds)))
          case None =>
            Some(bounds)
      catch
        case error: TypeError =>
          errorTrees.appendAll(error.trees)
          result
    )

    result match
      case Some(bounds) =>
        bounds
      case None =>
        throw TypeError(None, errorTrees.toList)

  /** Evaluate a typing function sequencially. */
  def seq(f: () => Context ?=> Clauses, ins: Clauses): Clauses =
    given Context = ctx.extend(ins)
    val outs = f()
    ins.concat(outs)
