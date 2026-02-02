package hkmc2.ctml.core

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

def inferSeq(expr: Expr, ins: Clauses)(using ctx: Context): (Type, Clauses) =
  given Context = ctx.extend(ins)
  val (type_, outs) = infer(expr)
  (type_, ins.concat(outs))

/** Infer the type of an expression. */
def infer(expr: Expr)(using ctx: Context): (Type, Clauses) =
  inferWithDebug(inferImpl)(expr)

/** Implementation of `constrainSub`. */
def inferImpl(expr: Expr)(using ctx: Context): (Type, Clauses) =
  expr match
    // Variable.
    case var_ : EVar =>
      (ctx.getVarType(var_.name), Clauses.empty)

    // Tuple introduction.
    case tuple: ETuple =>
      val (leftType,  leftClauses)  = infer(tuple.left)
      val (rightType, rightClauses) = inferSeq(tuple.right, leftClauses)
      (TTuple(leftType, rightType), rightClauses)

    // Lambda abstraction.
    case lam: ELam =>
      ctx.withFreshVarLevel((paramVar, ctx) =>
        val paramType = TVar(paramVar)
        given Context = ctx.extend(TermVarDecl(lam.paramName, paramType))
        val (bodyType, bodyClauses) = infer(lam.body)
        (TLam(paramType, bodyType), bodyClauses)
      )

    // Lambda application.
    case app: EApp =>
      ctx.withFreshVarLevel((retVar, ctx) =>
        given Context = ctx
        val (lamType, lamClauses) = infer(app.lam)
        val (argType, argClauses) = inferSeq(app.arg, lamClauses)
        val retType = TVar(retVar)
        val mockLamType = TLam(argType, retType)
        val consrainClauses = typingSubtypeSeq(lamType, mockLamType, argClauses)
        (retType, consrainClauses)
      )

    // Type ascription.
    case ascr: EAscr =>
      val (inferType, inferClauses) = infer(ascr.expr)
      val constrainClauses = typingSubtypeSeq(inferType, ascr.type_, inferClauses)
      (ascr.type_, constrainClauses)

    // Match.
    case match_ : EMatch =>
      inferMatch(match_)

/** Infer the type of a match expression. */
def inferMatch(match_ : EMatch)(using ctx: Context): (Type, Clauses) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeClauses) = infer(match_.scrutinee)
  // Get the union of the cases.
  val patternsType = match_.cases
    .map(_.pattern)
    .joinManySeq(scrutineeClauses)(using ctx, SubtypingCache())
  // Constrain the type of the scrutinee to be a subtype of the type of the cases.
  val patternsClauses = typingSubtypeSeq(scrutineeType, patternsType, scrutineeClauses)
  val patternsCtx = ctx.extend(patternsClauses)
  // Infer each match case.

  // Create a new fresh type variable for the type of the match expression.
  val (casesType, casesClauses) = patternsCtx.withFreshVarLevel((casesVar, casesCtx) =>
    val casesType = TVar(casesVar)
    val casesClauses = match_.cases
      .map(case_ =>
        given Context = casesCtx
        inferMatchCase(case_, scrutineeType, casesType)
      )
      .fold1Right((caseClauses, casesClauses) =>
        Clauses(casesCtx.joinBounds(caseClauses, casesClauses))
      )

    (casesType, casesClauses)
  )

  (casesType, patternsClauses.concat(casesClauses))

/** Infer the type of a match case. */
def inferMatchCase(case_ : EMatchCase, scrutineeType: Type, casesType: Type)(using ctx: Context): Clauses =
  if !config.weirdMatch then
    checkPattern(case_.pattern)

  val patternClauses =
    typingSubtype(scrutineeType, case_.pattern)
  val (bodyType, bodyClauses) = inferSeq(case_.body, patternClauses)
  typingSubtypeSeq(bodyType, casesType, bodyClauses)

def checkPattern(pattern: Type)(using ctx: Context) =
  pattern match
    case TVar(var_) if var_.isClass =>
      ()
    case _ =>
      throw TypeError(Some(s"Pattern ${pattern} is not a class."))

def typingSubtype(sub: Type, sup: Type)(using ctx: Context) =
  subtype(sub, sup)(using ctx, SubtypingCache())

def typingSubtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context) =
  subtypeSeq(sub, sup, ins)(using ctx, SubtypingCache())
