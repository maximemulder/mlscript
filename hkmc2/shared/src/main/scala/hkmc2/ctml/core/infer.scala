package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.syntax.Keyword.pattern

extension (ctx: Context)
  /** Evaluate a function within a context with a new fresh type variable. */
  def withFreshVarLevel(f: (String, Context) => (Type, List[Bound])): (Type, List[Bound]) =
    withFreshVar((varName, varCtx) =>
      val (type_ , bounds) = f(varName, varCtx)
      val newCtx = ctx.concatBounds(bounds)
      val polarities =
        given Polarity = Polarity.Positive
        getVarPolarities(type_, varName)
      val (newType, newBounds) = polarities match
        case Polarities(true, true) =>
          // TODO: Polymorphism.
          (TConstrained(List(varName), type_, Nil), bounds)
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
          val ctx1 = ctx.concatContext(freshVarCtx, varCtx)
          val (bodyType, bodyBounds) = infer(lam.body, ctx1)
          (TLam(var_, bodyType), bodyBounds)
        )
      )

    // Lambda application
    case app: EApp =>
      ctx.withFreshVarLevel((mockRetVarName, mockRetCtx) =>
        val mockCtx = ctx.concatContext(mockRetCtx)
        val (lamType, lamBounds) = infer(app.lam, mockCtx)
        val (argType, argBounds) = infer(app.arg, mockCtx)
        val mockLamType = TLam(argType, TVar(mockRetVarName))

        val inferBounds =
          given Context = ctx.concatContext(mockRetCtx).concatBounds(lamBounds, argBounds)
          constrainSub(lamType, mockLamType)
        (TVar(mockRetVarName), inferBounds ::: argBounds ::: lamBounds)
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferBounds) = infer(ascr.expr, ctx)
      val constrainBounds =
        given Context = ctx.concatBounds(inferBounds)
        constrainSub(inferType, ascr.type_)
      (ascr.type_, constrainBounds ::: inferBounds)

    case match_ : EMatch =>
      inferMatch(match_, ctx)

/** Infer the type of a match expression. */
def inferMatch(match_ : EMatch, ctx: Context): (Type, List[Bound]) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeBounds) = infer(match_.scrutinee, ctx)
  val scrutineeCtx = ctx.concatBounds(scrutineeBounds)
  // Get the union of the cases.
  val patternsType =
    given Context = scrutineeCtx
    joinMany(match_.cases.map(_.pattern))
  // Constrain the type of the scrutinee to be a subtype of the type of the cases.
  val patternsBounds =
    given Context = scrutineeCtx
    given Mode = Mode.Constrain
    constrainSub(scrutineeType, patternsType)
  val patternsCtx = scrutineeCtx.concatBounds(patternsBounds)
  // Infer each match case.
  given Context = patternsCtx
  val (casesType, casesBounds) = match_.cases
    .map(inferMatchCase(_, scrutineeType, patternsCtx))
    .fold1Right((case_, cases) =>
      val (caseType, caseBounds) = case_
      val (casesType, casesBounds) = cases
      val type_ = join(caseType, casesType)
      val bounds = patternsCtx.joinBounds(caseBounds, casesBounds)
      (type_, bounds)
    )

  (casesType, casesBounds ::: patternsBounds ::: scrutineeBounds)

/** Infer the type of a match case. */
def inferMatchCase(case_ : EMatchCase, scrutineeType: Type, ctx: Context): (Type, List[Bound]) =
  val patternBounds =
    given Context = ctx
    constrainSub(scrutineeType, case_.pattern)
  val caseCtx = ctx.concatBounds(patternBounds)
  val (bodyType, bodyBounds) = infer(case_.body, caseCtx)
  (bodyType, bodyBounds ::: patternBounds)
