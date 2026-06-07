package hkmc2.ctml.core.inference

import hkmc2.ctml.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.core.var_.declInferVar

def inferSeq(expr: Expr, ins: Clauses)(using ctx: Context): (Type, Clauses) =
  ctx.seq(infer(expr), ins)

def inferTopLevel(expr: Expr)(using ctx: Context): (Type, Clauses) =
  ctx.withInferLevel(() => infer(expr))

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
      ctx.withInferLevel(() =>
        val paramVarDecl = ctx.declInferVar()
        val paramType = TVar(paramVarDecl.var_)
        given Context = ctx.extend(paramVarDecl, TermVarDecl(lam.paramName, paramType))
        val (bodyType, bodyClauses) = infer(lam.body)
        (TLam(paramType, bodyType), Clauses.single(paramVarDecl).concat(bodyClauses))
      )

    // Lambda application.
    case app: EApp =>
      val (lamType, lamClauses) = infer(app.lam)
      val (argType, argClauses) = inferSeq(app.arg, lamClauses)
      val retVarDecl = ctx.declInferVar()
      val retType = TVar(retVarDecl.var_)
      val mockLamType = TLam(argType, retType)
      val consrainClauses = typingSubtypeSeq(lamType, mockLamType, argClauses.concat(retVarDecl.asClauses))
      (retType, consrainClauses)

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
    summon[Context].withInferLevel(() =>
      val ctx = summon[Context]
      val matchVarDecl = ctx.declInferVar()
      val matchType = TVar(matchVarDecl.var_)
      val matchCtx = ctx.extend(matchVarDecl)
      val (a, b) = (() =>
        given Context = matchCtx
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
      )()
      (a, Clauses.single(matchVarDecl).concat(b))
    ),
    //summon[Context].withInferenceLevel2((matchVar, matchCtx) =>
    //  given Context = matchCtx
    //  val matchType = TVar(matchVar)
    //  val patternClauses = typingSubtype(scrutineeType, match_.pattern)
    //  val (bodyType, bodyClauses) = inferSeq(match_.then_, patternClauses)
    //  val realBodyClauses = typingSubtypeSeq(bodyType, matchType, bodyClauses)
//
    //  match_.else_ match
    //    case Some(else_) =>
    //      val elsePatternClauses = typingSubtype(scrutineeType, TNeg(match_.pattern))
    //      val (elseType, elseClauses) = inferSeq(else_, elsePatternClauses)
    //      val realElseClauses = typingSubtypeSeq(elseType, matchType, elseClauses)
    //      (matchType, Clauses(matchCtx.joinBounds(realBodyClauses, realElseClauses)))
    //    case None =>
    //      (matchType, realBodyClauses)
    //),
    scrutineeClauses,
  )

def typingSubtype(sub: Type, sup: Type)(using ctx: Context) =
  subtype(sub, sup)(using ctx, ConstraintMode.Solve)

def typingSubtypeSeq(sub: Type, sup: Type, ins: Clauses)(using ctx: Context) =
  subtypeSeq(sub, sup, ins)(using ctx, ConstraintMode.Solve)
