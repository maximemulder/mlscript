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
  (ctx.withInferLevel((ctx) => infer(expr)(using ctx)), SubClauses.empty)

/** Infer the type of an expression. */
def infer(expr: Expr)(using ctx: TypeContext): (Type, SubClauses) =
  inferWithDebug(inferImpl)(expr)

/** Implementation of `constrainSub`. */
def inferImpl(expr: Expr)(using ctx: TypeContext): (Type, SubClauses) =
  expr match
    case EUnit =>
      (TClass(ClassVar("Unit")), SubClauses.empty)

    // Variable.
    case var_ : EVar =>
      (ctx.getVarType(var_.variable), SubClauses.empty)

    // Tuple introduction.
    case tuple: ETuple =>
      val (leftType,  leftClauses)  = infer(tuple.left)
      val (rightType, rightClauses) = inferSeq(tuple.right, leftClauses)
      (TTuple(leftType, rightType), rightClauses)

    // Lambda abstraction.
    case lam: ELam =>
      val type_ = ctx.withInferLevel: innerCtx =>
        val (paramType, paramClauses, bodyCtx) = lam.param match
          case Some(param) =>
            val paramDecl = innerCtx.sub.declInferVar()
            val paramType = TVar(paramDecl.var_)
            val clauses = param.annotation.fold(paramDecl.asSubClauses): annotation =>
              typingSubtypeSeq(paramType, annotation, paramDecl.asSubClauses)(using innerCtx)
            val bodyCtx = innerCtx.extendSub(clauses)
              .extendTerm(TermVarDecl(param.variable, paramType))
            (paramType, clauses, bodyCtx)
          case None =>
            (TClass(ClassVar("Unit")), SubClauses.empty, innerCtx)
        val (bodyType, bodyClauses) = infer(lam.body)(using bodyCtx)
        (TLam(paramType, bodyType), paramClauses.concat(bodyClauses))
      // A parameter's fresh variable permits branch refinements. Its inferred domain must still
      // accept every value promised by the annotation, rather than just an inferred subset of it.
      val clauses = lam.param.flatMap(_.annotation).fold(SubClauses.empty): annotation =>
        typingSubtype(type_, TLam(annotation, TTop))
      (type_, clauses)

    // Lambda application.
    case app: EApp =>
      val (lamType, lamClauses) = infer(app.lam)
      val (argType, argClauses) = inferSeq(app.arg, lamClauses)
      val retDecl = ctx.sub.declInferVar()
      val retType = TVar(retDecl.var_)
      val clauses = typingSubtypeSeq(lamType, TLam(argType, retType),
        argClauses.concat(retDecl.asSubClauses))
      (retType, clauses)

    // Type ascription.
    case ascr: EAscr =>
      val (inferType, inferClauses) = infer(ascr.expr)
      val constrainClauses = typingSubtypeSeq(inferType, ascr.type_, inferClauses)
      (ascr.type_, constrainClauses)

    // Match.
    case match_ : EMatch =>
      inferMatch(match_)

/** Infer the type of a match expression.
 *
 *  A match with an else branch types each branch under the bound of the scrutinee by the pattern
 *  or by its negation, and joins the clauses of the two branches (see `joinBounds`). A match
 *  without an else branch must be exhaustive: the scrutinee is bounded by the pattern in the
 *  enclosing context, under which the then branch alone is typed (the paper's `I-IfThen`). The
 *  missing else branch is dead, so typing it as a branch that bounds the scrutinee by `⊥` and
 *  joining it with the then branch only adds the solutions where the scrutinee is `⊥`, which are
 *  vacuous for a parameter, while its guard `{α ≤ ⊥} ⟹ ⊥` would be nested in the joined bound of
 *  every variable of the then branch, and these bounds in the guards of every enclosing join. */
def inferMatch(match_ : EMatch)(using ctx: TypeContext): (Type, SubClauses) =
  // Infer the type and bounds of the scrutinee.
  val (scrutineeType, scrutineeClauses) = infer(match_.scrutinee)
  if !config.arbitraryPatterns && !match_.pattern.isPattern then
    throw TypeError(Some(s"Pattern ${match_.pattern} is not a class."))

  match_.else_ match
    case Some(else_) =>
      ctx.seq(inferMatchBranches(scrutineeType, match_.pattern, match_.then_, else_),
        scrutineeClauses)
    case None =>
      val patternClauses = typingSubtypeSeq(scrutineeType, match_.pattern, scrutineeClauses)
      inferSeq(match_.then_, patternClauses)

/** Infer both branches in a context containing the scrutinee's generated declarations and bounds.
 *  This scope matters for a computed scrutinee: its result variable is declared by inference of
 *  the scrutinee rather than by an enclosing parameter or UCS binding. */
private def inferMatchBranches(scrutineeType: Type, pattern: Type, then_ : Expr, else_ : Expr)
  (using ctx: TypeContext): (Type, SubClauses) =
  val matchVarDecl = ctx.sub.declInferVar()
  val matchType = TVar(matchVarDecl.var_)
  val matchCtx = ctx.extendSub(matchVarDecl)
  val clauses =
    given TypeContext = matchCtx
    val patternClauses = typingSubtype(scrutineeType, pattern)
    val (bodyType, bodyClauses) = inferSeq(then_, patternClauses)
    val realBodyClauses = typingSubtypeSeq(bodyType, matchType, bodyClauses)
    val elsePatternClauses = typingSubtype(scrutineeType, TNeg(pattern))
    val (elseType, elseClauses) = inferSeq(else_, elsePatternClauses)
    val realElseClauses = typingSubtypeSeq(elseType, matchType, elseClauses)
    SubClauses(matchCtx.sub.joinBounds(realBodyClauses, realElseClauses))
  (matchType, SubClauses.single(matchVarDecl).concat(clauses))

def typingSubtype(sub: Type, sup: Type)(using ctx: TypeContext) =
  subtype(sub, sup)(using ctx.sub)

def typingSubtypeSeq(sub: Type, sup: Type, ins: SubClauses)(using ctx: TypeContext) =
  subtypeSeq(sub, sup, ins)(using ctx.sub)
