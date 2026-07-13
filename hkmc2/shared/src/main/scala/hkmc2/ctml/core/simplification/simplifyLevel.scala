package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.types.*
import hkmc2.ctml.core.type_.impls.getVarPolarities.getVarPolarities
import hkmc2.ctml.core.context.extend
import hkmc2.ctml.core.structural.getTransDeps
import hkmc2.ctml.core.inference.inlineVar
import hkmc2.ctml.core.context.getTypeVarEffectiveLevel
import hkmc2.ctml.core.structural.getDeps

extension (ctx: SubContext)
  // Simplify the type and output clauses at a given level without closing that level.
  def simplifyLevel(type1 : Type, level: Int, outs1: SubClauses): (Type, SubClauses) =
    val (type2, outs2) = ctx.simplifyClauses(type1, level, outs1)

    val type3 = type2.simplify()(using ctx.extend(outs2), NoInlineVars(Set()))
    val outs3 = outs2

    (type3, outs3)
