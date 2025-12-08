package hkmc2.ctml.core.context

import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Freeze a typing context by replacing all the flexible type variables by rigid type
   *  variables. */
  def freeze(): Context =
    ctx.mapClauses(clause =>
      clause match
        case TypeVarDecl(var_, TypeVarKind.Flex, original) =>
          TypeVarDecl(var_, TypeVarKind.Rigid, original)
        case _ =>
          clause
    )
