package hkmc2.ctml.core.inference

import hkmc2.ctml.config.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*
import hkmc2.ctml.core.var_.declInferVar

def inferSeq(expr: Expr, ins: SubClauses)(using ctx: TypeContext): (Type, SubClauses) =
  ctx.seq(infer(expr), ins)

def inferTopLevel(expr: Expr)(using ctx: TypeContext): (Type, SubClauses) =
  ctx.withInferLevel((ctx) => infer(expr)(using ctx))

/** Infer the type of an expression. */
def infer(expr: Expr)(using ctx: TypeContext): (Type, SubClauses) =
  inferWithDebug(inferImpl)(expr)

/** Implementation of `constrainSub`. */
def inferImpl(expr: Expr)(using ctx: TypeContext): (Type, SubClauses) =
  expr match
    // Variable.
    case var_ : EVar =>
      (ctx.getVarType(var_.name), SubClauses.empty)

    // Tuple introduction.
    case tuple: ETuple =>
      val (leftType,  leftClauses)  = infer(tuple.left)
      val (rightType, rightClauses) = inferSeq(tuple.right, leftClauses)
      (TTuple(leftType, rightType), rightClauses)

    // Lambda abstraction.
    case lam: ELam =>
      ctx.withInferLevel((ctx) =>
        val paramVarDecl = ctx.sub.declInferVar()
        val paramType = TVar(paramVarDecl.var_)
        given TypeContext = ctx.extendSub(paramVarDecl).extendTerm(TermVarDecl(lam.paramName, paramType))
        val (bodyType, bodyClauses) = infer(lam.body)
        (TLam(paramType, bodyType), SubClauses.single(paramVarDecl).concat(bodyClauses))
      )

    // Lambda application.
    case app: EApp =>
      val (lamType, lamClauses) = infer(app.lam)
      val (argType, argClauses) = inferSeq(app.arg, lamClauses)
      val retVarDecl = ctx.sub.declInferVar()
      val retType = TVar(retVarDecl.var_)
      val mockLamType = TLam(argType, retType)
      val consrainClauses = typingSubtypeSeq(lamType, mockLamType, argClauses.concat(retVarDecl.asSubClauses))
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
def inferMatch(match_ : EMatch)(using ctx: TypeContext): (Type, SubClauses) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeClauses) = infer(match_.scrutinee)
  if !config.arbitraryPatterns && !match_.pattern.isPattern then
    throw TypeError(Some(s"Pattern ${match_.pattern} is not a class."))

  ctx.seq(
    summon[TypeContext].withInferLevel((ctx) =>
      val matchVarDecl = ctx.sub.declInferVar()
      val matchType = TVar(matchVarDecl.var_)
      val matchCtx = ctx.extendSub(matchVarDecl)
      val (a, b) =
        given TypeContext = matchCtx
        val patternClauses = typingSubtype(scrutineeType, match_.pattern)
        val (bodyType, bodyClauses) = inferSeq(match_.then_, patternClauses)
        val realBodyClauses = typingSubtypeSeq(bodyType, matchType, bodyClauses)

        match_.else_ match
          case Some(else_) =>
            val elsePatternClauses = typingSubtype(scrutineeType, TNeg(match_.pattern))
            val (elseType, elseClauses) = inferSeq(else_, elsePatternClauses)
            val realElseClauses = typingSubtypeSeq(elseType, matchType, elseClauses)
            (matchType, SubClauses(matchCtx.sub.joinBounds(realBodyClauses, realElseClauses)))
          case None =>
            val elsePatternClauses = typingSubtype(scrutineeType, TNeg(match_.pattern))
            val realElseClauses = typingSubtypeSeq(scrutineeType, TBot, elsePatternClauses)
            (matchType, SubClauses(matchCtx.sub.joinBounds(realBodyClauses, realElseClauses)))
      (a, SubClauses.single(matchVarDecl).concat(b))
    ),
    scrutineeClauses,
  )

def typingSubtype(sub: Type, sup: Type)(using ctx: TypeContext) =
  subtype(sub, sup)(using ctx.sub, ConstraintMode.Solve)

def typingSubtypeSeq(sub: Type, sup: Type, ins: SubClauses)(using ctx: TypeContext) =
  subtypeSeq(sub, sup, ins)(using ctx.sub, ConstraintMode.Solve)
