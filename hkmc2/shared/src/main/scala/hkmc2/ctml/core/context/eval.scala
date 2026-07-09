package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.types.*
import scala.collection.mutable.ListBuffer

// Methods used to evaluate conjunctions and disjunctions within a subtyping context.

extension (ctx: SubContext)
  /** Evaluate some functions and meet the bounds returned. */
  def all(fs: (SubContext ?=> SubClauses)*): SubClauses =
    // A left fold preserves the arguments order if they are passed from left to right, which
    // should be the case if they are statically written inline, but should not the case if they.
    // come from a dynamically generated list.
    fs.foldLeft(SubClauses.empty)((clauses, f) => ctx.seqUnit(f, clauses))

  /** Evaluate some functions and join the bounds returned. */
  def any(fs: (SubContext ?=> SubClauses)*): SubClauses =
    val errorTrees = ListBuffer[ProofTree]()

    // A left fold preserves the arguments order if they are passed from left to right, which
    // should be the case if they are statically written inline, but should not the case if they.
    // come from a dynamically generated list.
    val result = fs.foldLeft(None: Option[SubClauses])((result, f) =>
      try
        given SubContext = ctx
        val bounds = f
        result match
          case Some(resultBounds) =>
            Some(SubClauses(ctx.joinBounds(resultBounds, bounds)))
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

  /** Sequentially evaluate a function in a subtyping context and get its return value. */
  def seq[T](f: SubContext ?=> (T, SubClauses), ins: SubClauses): (T, SubClauses) =
    given SubContext = ctx.extend(ins)
    val (result, outs) = f
    (result, ins.concat(outs))

  /** Sequentially evaluate a function in a subtyping context. */
  def seqUnit(f: SubContext ?=> SubClauses, ins: SubClauses): SubClauses =
    ctx.seq(((), f), ins)._2

extension (ctx: TypeContext)
  /** Sequentially evaluate a typing function while extending only the subtyping context. */
  def seq[T](f: TypeContext ?=> (T, SubClauses), ins: SubClauses): (T, SubClauses) =
    given TypeContext = ctx.extendSub(ins)
    val (result, outs) = f
    (result, ins.concat(outs))
