package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Infer the type of an expression. */
def infer(expr: Expr, ctx: Context): (Type, List[Bound]) =
  expr match
    // Variable
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Nil)

    // Lambda abstraction
    case lam: ELam =>
      ctx.withFreshVar((varName, ctx) =>
        val var_ = TVar(varName)
        ctx.withVar(lam.paramName, var_, (ctx) =>
          val (bodyType, bodyBounds) = infer(lam.body, ctx)
          (TLam(var_, bodyType), bodyBounds)
        )
      )

    // Lambda application
    case app: EApp =>
      ctx.withFreshVar((mockRetVarName, mockRetCtx) =>
        val (lamType, lamBounds) = infer(app.lam, mockRetCtx)
        val (argType, argBounds) = infer(app.arg, mockRetCtx)
        val mockLamType = TLam(argType, TVar(mockRetVarName))

        val inferBounds = inferConstrainSub(lamType, mockLamType, mockRetCtx)
        (TVar(mockRetVarName), inferBounds ++ argBounds ++ lamBounds)
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferBounds) = infer(ascr.expr, ctx)
      val constrainBounds = inferConstrainSub(inferType, ascr.type_, ctx)
      (ascr.type_, constrainBounds ++ inferBounds)

/** Constrain a type to be a subtype of another in type inference, or throw an exception if that
 * relation cannot be satisfied. */
def inferConstrainSub(sub: Type, sup: Type, ctx: Context): List[Bound] =
  given Context = ctx
  given Mode = Mode.Constrain
  constrainSub(sub, sup)
