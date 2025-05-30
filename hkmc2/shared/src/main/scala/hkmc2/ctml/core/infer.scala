package hkmc2.ctml.core

import hkmc2.ctml.core.merge.*
import hkmc2.ctml.types.*

/** Infer the type of an expression. */
def infer(expr: Expr, ctx: Clauses): (Type, Clauses) =
  expr match
    // Variable
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Clauses.none)

    // Lambda abstraction
    case lam: ELam =>
      ctx.withLevel(ctx =>
        val freshVar = newInferFreshVar()
        val paramVar = TVar(freshVar.name)
        val paramCtx = ctx.addClause(freshVar, TermVar(lam.paramName, paramVar))
        val (bodyType, bodyBounds) = infer(lam.body, paramCtx)
        (TLam(paramVar, bodyType), Clauses(List(freshVar)).addClauses(bodyBounds))
      )

    // Lambda application
    case app: EApp =>
      ctx.withLevel(ctx =>
        val freshVar = newInferFreshVar()
        val freshCtx = ctx.addClause(freshVar)
        val (lamType, lamClauses) = infer(app.lam, freshCtx)
        val (argType, argClauses) = infer(app.arg, freshCtx)
        val retVar = TVar(freshVar.name)
        val mockLamType = TLam(argType, retVar)
        val consrainClauses =
          given Clauses = freshCtx.addClauses(lamClauses, argClauses)
          constrainSub(lamType, mockLamType)
        (TVar(retVar.name), Clauses(List(freshVar)).addClauses(lamClauses, argClauses, consrainClauses))
      )

    // Type ascription
    case ascr: EAscr =>
      val (inferType, inferClauses) = infer(ascr.expr, ctx)
      val constrainClauses =
        given Clauses = ctx.addClauses(inferClauses)
        constrainSub(inferType, ascr.type_)
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
    constrainSub(scrutineeType, patternsType)
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
    constrainSub(scrutineeType, case_.pattern)
  val caseCtx = ctx.addClauses(patternClauses)
  val (bodyType, bodyClauses) = infer(case_.body, caseCtx)
  (bodyType, patternClauses.addClauses(bodyClauses))
