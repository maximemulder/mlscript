package hkmc2.ctml.core.context

import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Rigidify the subtyping context by replacing all flexible type variables with rigid type
   *  variables. */
  def rigidify(): SubContext =
    ctx.mapClauses(clause =>
      clause match
        case TypeVarDecl(var_, TypeVarKind.Flex, original, level) =>
          TypeVarDecl(var_, TypeVarKind.Rigid, original, level)
        case _ =>
          clause
    )

  /** Flexify the subtyping context by replacing all rigid type variables with flexible type
   *  variables. */
  def flexify(): SubContext =
    ctx.mapClauses(_ match
      case TypeVarDecl(var_, TypeVarKind.Rigid, original, level) =>
        TypeVarDecl(var_, TypeVarKind.Flex, original, level)
      case clause =>
        clause
    )
