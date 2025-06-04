package hkmc2.ctml.core

import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

/** Infer the type of an expression. */
def infer(expr: Expr, ctx: Clauses): (Type, Clauses) =
  inferImpl(expr, ctx)

/** Implementation of `constrainSub`. */
def inferImpl(expr: Expr, ctx: Clauses): (Type, Clauses) =
  expr match
    // Variable
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Clauses.none)

    // Lambda abstraction
    case lam: ELam =>
      ctx.withFreshVarLevel((paramVar, ctx) =>
        val paramCtx = ctx.addClause(TermVar(lam.paramName, paramVar))
        val (bodyType, bodyBounds) = infer(lam.body, paramCtx)
        (TLam(paramVar, bodyType), bodyBounds)
      )

    // Lambda application
    case app: EApp =>
      ctx.withFreshVarLevel((retVar, ctx) =>
        val (lamType, lamClauses) = infer(app.lam, ctx)
        val (argType, argClauses) = infer(app.arg, ctx)
        val mockLamType = TLam(argType, retVar)
        val consrainClauses =
          given Clauses = ctx.addClauses(lamClauses, argClauses)
          subtype(lamType, mockLamType)
        (retVar, lamClauses.addClauses(argClauses, consrainClauses))
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferClauses) = infer(ascr.expr, ctx)
      val constrainClauses =
        given Clauses = ctx.addClauses(inferClauses)
        subtype(inferType, ascr.type_)
      (ascr.type_, constrainClauses.addClauses(inferClauses))

    case match_ : EMatch =>
      inferMatch(match_, ctx)

/** Infer the type of a match expression. */
def inferMatch(match_ : EMatch, ctx: Clauses): (Type, Clauses) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeClauses) = infer(match_.scrutinee, ctx)
  val scrutineeCtx = ctx.addClauses(scrutineeClauses)
  // Get the union of the cases.
  val patternsType =
    given Clauses = scrutineeCtx
    match_.cases
      .map(_.pattern)
      .joinMany()
  // Constrain the type of the scrutinee to be a subtype of the type of the cases.
  val patternsClauses =
    given Clauses = scrutineeCtx
    subtype(scrutineeType, patternsType)
  val patternsCtx = scrutineeCtx.addClauses(patternsClauses)
  // Infer each match case.
  given Clauses = patternsCtx
  val (casesType, casesClauses) = match_.cases
    .map(inferMatchCase(_, scrutineeType, patternsCtx))
    .fold1Right((case_, cases) =>
      val (caseType, caseBounds) = case_
      val (casesType, casesBounds) = cases
      val type_ = join(caseType, casesType)
      val bounds = Clauses(patternsCtx.joinBounds(caseBounds, casesBounds))
      (type_, bounds)
    )

  (casesType, scrutineeClauses.addClauses(patternsClauses, casesClauses))

/** Infer the type of a match case. */
def inferMatchCase(case_ : EMatchCase, scrutineeType: Type, ctx: Clauses): (Type, Clauses) =
  val patternClauses =
    given Clauses = ctx
    subtype(scrutineeType, case_.pattern)
  val caseCtx = ctx.addClauses(patternClauses)
  val (bodyType, bodyClauses) = infer(case_.body, caseCtx)
  (bodyType, patternClauses.addClauses(bodyClauses))
