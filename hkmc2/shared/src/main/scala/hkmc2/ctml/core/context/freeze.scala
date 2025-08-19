package hkmc2.ctml.core.context

import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Freeze a typing context by replacing all fresh type variables with rigid type variables. */
  def freeze(): Context =
    Context(ctx.clauses.map(clause =>
      clause match
        case TypeVarDecl(var_, TypeVarKind.Fresh) =>
          TypeVarDecl(var_, TypeVarKind.Rigid)
        case _ =>
          clause
    ))
