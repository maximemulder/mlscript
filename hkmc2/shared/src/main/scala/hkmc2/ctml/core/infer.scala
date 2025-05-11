package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.semantics.Term
import hkmc2.semantics.Term.*
import hkmc2.syntax.Keyword
import hkmc2.syntax.Tree.*

/** Infer the type of a term. */
def infer(ctx: Context, term: Term): (Type, List[Bound]) =
  term match
    // Ignore the statements of MLScript block and only type the final term.

    case Blk(_, term) =>
      infer(ctx, term)

    // Type MLScript literals using type variables.

    case UnitVal() | Lit(UnitLit(_)) =>
      (TVar("Unit"), Nil)
    case Lit(BoolLit(_)) =>
      (TVar("Bool"), Nil)
    case Lit(IntLit(_)) =>
      (TVar("Int"), Nil)
    case Lit(DecLit(_)) =>
      (TVar("Decimal"), Nil)
    case Lit(StrLit(_)) =>
      (TVar("String"), Nil)

    // Variable

    case Ref(symbol) =>
      val varName = symbol.nme
      (ctx.getVarType(varName), Nil)

    // Lambda abstraction

    case Lam(params, body) =>
      val paramsCount = params.paramCountLB
      if paramsCount != 1 then
        throw new ParseError(term)

      var paramName = params.allParams(0).sym.name

      ctx.withFreshVar((varName, ctx) =>
        val var_ = TVar(varName)
        ctx.withVar(paramName, var_, (ctx) =>
          val (bodyType, bodyBounds) = infer(ctx, body)
          (TFun(var_, bodyType), bodyBounds)
        )
      )

    // Lambda application

    case Term.App(fun, arg) =>
      ctx.withFreshVar((mockRetVarName, mockRetCtx) =>
        val (funType, funBounds) = infer(mockRetCtx, fun)
        val (argType, argBounds) = infer(mockRetCtx, arg)
        val mockFunType = TFun(argType, TVar(mockRetVarName))

        val inferBounds = inferConstrainSub(mockRetCtx, funType, mockFunType)
        (TVar(mockRetVarName), inferBounds ++ argBounds ++ funBounds)
      )

    // Type ascription

    case Term.Asc(term, typeNode) =>
      val type_ = typeNode.toType()
      val (inferType, inferBounds) = infer(ctx, term)
      val constrainBounds = inferConstrainSub(ctx, inferType, type_)
      (type_, constrainBounds ++ inferBounds)

    // Match expression

    case Term.IfLike(Keyword.`if`, _) =>
      // TODO: How to desugar a match expression ?
      (TBot, Nil)

    case _ =>
      throw new ParseError(term)

/** Constrain a type to be a subtype of another in type inference, or throw an exception if that
 * relation cannot be satisfied. */
def inferConstrainSub(ctx: Context, sub: Type, sup: Type): List[Bound] =
  given Context = ctx
  given Mode = Mode.Constrain
  constrainSub(sub, sup)
