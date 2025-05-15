package hkmc2.ctml.core

import hkmc2.ctml.types.*

extension (ctx: Context)
  /** Evaluate a function within a context with a new fresh type variable. */
  def withFreshVarLevel(f: (String, Context) => (Type, List[Bound])): (Type, List[Bound]) =
    withFreshVar((varName, varCtx) =>
      val (type_ , bounds) = f(varName, varCtx)
      val newCtx = concatCtxs(ctx, bounds.c)
      val polarities =
        given Polarity = Polarity.Positive
        getVarPolarities(type_, varName)
      val (newType, newBounds) = polarities match
        case Polarities(true, true) =>
          // TODO: Polymorphism.
          (type_, bounds)
        case Polarities(true, false) =>
          val upperBound = newCtx.getVarUpperBound(varName)
          given Context = newCtx
          val newType = substitute(type_, varName, upperBound)
          (newType, bounds)
        case Polarities(false, true) =>
          val lowerBound = newCtx.getVarLowerBound(varName)
          given Context = newCtx
          val newType = substitute(type_, varName, lowerBound)
          (newType, bounds)
        case Polarities(false, false) =>
          (type_, bounds)
      (newType, newBounds.removeVar(varName))
    )

/** Infer the type of an expression. */
def infer(expr: Expr, ctx: Context): (Type, List[Bound]) =
  expr match
    // Variable
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Nil)

    // Lambda abstraction
    case lam: ELam =>
      ctx.withFreshVarLevel((freshVarName, freshVarCtx) =>
        val var_ = TVar(freshVarName)
        withVar(lam.paramName, var_, (varCtx) =>
          val ctx1 = concatCtxs(varCtx, freshVarCtx, ctx)
          val (bodyType, bodyBounds) = infer(lam.body, ctx1)
          (TLam(var_, bodyType), bodyBounds)
        )
      )

    // Lambda application
    case app: EApp =>
      ctx.withFreshVarLevel((mockRetVarName, mockRetCtx) =>
        val ctx1 = concatCtxs(mockRetCtx, ctx)
        val (lamType, lamBounds) = infer(app.lam, ctx1)
        val (argType, argBounds) = infer(app.arg, ctx1)
        val mockLamType = TLam(argType, TVar(mockRetVarName))

        val ctx2 = concatCtxs(argBounds.c, lamBounds.c, ctx1)
        val inferBounds = inferConstrainSub(lamType, mockLamType, ctx2)
        (TVar(mockRetVarName), inferBounds ::: argBounds ::: lamBounds)
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferBounds) = infer(ascr.expr, ctx)
      val ctx1 = concatCtxs(inferBounds.c, ctx)
      val constrainBounds = inferConstrainSub(inferType, ascr.type_, ctx1)
      (ascr.type_, constrainBounds ::: inferBounds)

    // Pattern matching
    case match_ : EMatch =>
      throw new TypeError("TODO")

/** Constrain a type to be a subtype of another in type inference, or throw an exception if that
 * relation cannot be satisfied. */
def inferConstrainSub(sub: Type, sup: Type, ctx: Context): List[Bound] =
  given Context = ctx
  given Mode = Mode.Constrain
  constrainSub(sub, sup)
