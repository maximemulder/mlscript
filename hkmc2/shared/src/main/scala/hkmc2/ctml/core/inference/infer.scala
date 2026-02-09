package hkmc2.ctml.core.inference

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.system.*
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
  if !config.weirdMatch && !match_.pattern.isPattern then
    throw TypeError(Some(s"Pattern ${match_.pattern} is not a class."))

  val scrutineeCtx = ctx.extend(scrutineeClauses)
  val (casesType, casesClauses) = scrutineeCtx.withFreshVarLevel((matchVar, matchCtx) =>
    given Context = matchCtx
    val matchType = TVar(matchVar)
    val patternClauses = typingSubtype(scrutineeType, match_.pattern)
    val (bodyType, bodyClauses) = inferSeq(match_.then_, patternClauses)
    val realBodyClauses = typingSubtypeSeq(bodyType, matchType, bodyClauses)

    match_.else_ match
      case Some(else_) =>
        val elsePatternClauses = typingSubtype(scrutineeType, TNeg(match_.pattern))
        val (elseType, elseClauses) = inferSeq(else_, elsePatternClauses)
        val realElseClauses = typingSubtypeSeq(elseType, matchType, elseClauses)
        (matchType, Clauses(matchCtx.joinBounds(realBodyClauses, realElseClauses)))
      case None =>
        (matchType, realBodyClauses)
  )

  (casesType, scrutineeClauses.concat(casesClauses))

def typingSubtype(sub: Type, sup: Type)(using ctx: Context) =
  subtype(sub, sup)(using ctx, SubtypingCache())

def typingSubtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context) =
  subtypeSeq(sub, sup, ins)(using ctx, SubtypingCache())
