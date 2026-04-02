package hkmc2.ctml.core.inference

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

def inferSeq(expr: Expr, ins: Clauses)(using ctx: Context): (Type, Clauses) =
  ctx.seq(infer(expr), ins)

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
      val (lamType, lamClauses) = infer(app.lam)
      val (argType, argClauses) = inferSeq(app.arg, lamClauses)
      ctx.seq(
        summon[Context].withFreshVarLevel((retVar, ctx) =>
          val retType = TVar(retVar)
          val mockLamType = TLam(argType, retType)
          given Context = ctx
          val consrainClauses = typingSubtype(lamType, mockLamType)
          (retType, consrainClauses)
        ),
        argClauses,
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
  if !config.arbitraryPatterns && !match_.pattern.isPattern then
    throw TypeError(Some(s"Pattern ${match_.pattern} is not a class."))

  ctx.seq(
    summon[Context].withFreshVarLevel((matchVar, matchCtx) =>
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
    ),
    scrutineeClauses,
  )

def typingSubtype(sub: Type, sup: Type)(using ctx: Context) =
  subtype(sub, sup)(using ctx, SubtypingCache())

def typingSubtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context) =
  subtypeSeq(sub, sup, ins)(using ctx, SubtypingCache())
