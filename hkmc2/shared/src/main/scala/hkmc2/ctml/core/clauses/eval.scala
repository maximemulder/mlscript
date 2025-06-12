package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

// Methods used to evaluate some functions within a context.

extension (ctx: Clauses)
  /** Evaluate some functions and meet the bounds returned. */
  def all(fs: (() => Clauses)*): Clauses =
    Clauses(fs.flatMap(_().elems).toList)

  /** Evaluate some functions and join the bounds returned. */
  def any(fs: (() => Clauses)*): Clauses =
    val errorTrees = ListBuffer[ProofTree]()

    val result = fs.foldRight(None: Option[Clauses])((f, result) =>
      try
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
