package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.structural.getTransDeps
import hkmc2.ctml.core.inference.inlineVar
import hkmc2.ctml.core.context.getTypeVarEffectiveLevel

extension (ctx: SubContext)
  // Simplify the type and output clauses at a given level without closing that level.
  def simplifyLevel(type1 : Type, level: Int, outs1: SubClauses): (Type, SubClauses) =
    val levelVars = outs1.levelVars(level)(using ctx)

    val (type2, outs2) = levelVars.foldRight(type1, outs1)((levelVar, to) =>
      simplifyVar(levelVar, to._1, level, to._2)
    )

    val type3 = type2.simplify()(using ctx.extend(outs2), NoInlineVars(Set()))
    val outs3 = outs2

    (type3, outs2)

  def simplifyVar(var_ : TypeVar, type_ : Type, level: Int, outs: SubClauses): (Type, SubClauses) =
    type_.getInlinePolarities(var_)(using ctx.extend(outs)) match
      case Some(inlinePols) =>
        // // TODO: Try to remove this check.
        // if !canEliminateDisconnectedVar(var_)(using ctx.extend(outs)) then
        //   return (type_, outs)

        inlineVar(type_, var_, inlinePols, outs)(using ctx)
      case None =>
        (type_, outs)

// TODO: Move stuff below
extension (clauses: SubClauses)
  def levelVars(level: Int)(using ctx: SubContext): List[TypeVar] =
    clauses.typeVarDecls.filter(_.level >= level)
      .map(_.var_)
      .filter(ctx.extend(clauses).getTypeVarEffectiveLevel(_) >= level)
