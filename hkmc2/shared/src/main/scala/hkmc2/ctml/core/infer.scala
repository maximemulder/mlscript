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
        val inferBounds =
          given Context = ctx2
          constrainSub(lamType, mockLamType)
        (TVar(mockRetVarName), inferBounds ::: argBounds ::: lamBounds)
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferBounds) = infer(ascr.expr, ctx)
      val ctx1 = concatCtxs(inferBounds.c, ctx)
      val constrainBounds =
        given Context = ctx1
        constrainSub(inferType, ascr.type_)
      (ascr.type_, constrainBounds ::: inferBounds)

    case match_ : EMatch =>
      inferMatch(match_, ctx)

/** Infer the type of a match expression. */
def inferMatch(match_ : EMatch, ctx: Context): (Type, List[Bound]) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeBounds) = infer(match_.expr, ctx)
  val ctx1 = concatCtxs(scrutineeBounds.c, ctx)
  // Get the union of the cases.
  val casesType =
    given Context = ctx1
    joinMany(match_.cases.map(_.type_))
  // Constrain the type of the scrutinee to be a subtype of the type of the cases.
  val constrainBounds =
    given Context = ctx1
    constrainSub(scrutineeType, casesType)
  // Infer each match case.
  val ctx2 = concatCtxs(constrainBounds.c, ctx1)
  val (casesType2, cases2Bounds) = match_.cases
    .map(inferMatchCase(_, scrutineeType, ctx2))
    .fold1Right((case_, cases) =>
      val (caseType, caseBounds) = case_
      val (casesType, casesBonds) = cases
      val caseConstrainingType =
        given Context = ctx2
        attachConstrainingBounds(caseType, caseBounds)
      val casesConstrainingType =
        given Context = ctx2
        join(casesType, caseConstrainingType)
      (casesConstrainingType, Nil)
    )

  (casesType2, constrainBounds ::: scrutineeBounds)

/** Infer the type of a match case. */
def inferMatchCase(case_ : EMatchCase, scrutineeType: Type, ctx: Context): (Type, List[Bound]) =
  val scrutineeBounds =
    given Context = ctx
    constrainSub(scrutineeType, case_.type_)
  val ctx3 = concatCtxs(ctx, scrutineeBounds.c)
  infer(case_.body, ctx3)
