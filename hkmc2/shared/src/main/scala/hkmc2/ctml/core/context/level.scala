package hkmc2.ctml.core.context

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.getDependentVars
import hkmc2.ctml.config.debug

extension (ctx: Context)
  /** Evaluate a function in a new level with a new fresh type variable and solve that level. */
  def withFreshVarLevel[T](
    kind: TypeVarKind,
    decls : List[TypeVarDecl],
    inner: (List[TypeVar], Context) => (T, Clauses),
    outer: (Int, T, Clauses) => (T, Clauses),
  ): (T, Clauses) =
    // Evaluate the inner function with the type variable in the context.
    val (res, innerOuts) = inner(decls.map(_.var_), ctx.extend(decls))

    // Move the type variable to the output clauses.
    val outs = Clauses(decls).concat(innerOuts)

    // Evaluate the outer function with the type variable in the output clauses.
    outer(decls(0).level, res, outs)

  /** Get the type variable with the highest declaration level in the context. */
  def getHighestLevelVar(): Option[TypeVar] =
    ctx.clauses.typeVars
      .maxByOption(ctx.getTypeVarLevel(_))

  /** Get the type variable with the highest effective level in the context. */
  def getHighestEffectiveLevelVar(): Option[TypeVar] =
    ctx.clauses.typeVars
      .maxByOption(ctx.getTypeVarEffectiveLevel(_))

  /** Get all the type variable with an effective levels equal or higher to this level. */
  def getLevelVars(level: Int): List[TypeVar] =
    val a = ctx.clauses.typeVars
      .sortWith(ctx.getTypeVarLevel(_) > ctx.getTypeVarLevel(_))
      .filter(ctx.getTypeVarLevel(_) >= level)
      .toList
    debug(s"LEVEL VARS ${level}: ${a}")
    a

  /** Get the effective level of a type variable. */
  def getTypeVarEffectiveLevel(var_ : TypeVar): Int =
    Iterator
      .single(var_)
      .concat(ctx.getDependentVars(var_))
      .map(ctx.getTypeVarLevel(_))
      .min
