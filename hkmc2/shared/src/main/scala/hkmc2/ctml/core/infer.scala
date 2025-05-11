package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.semantics.Term
import hkmc2.semantics.Term.*
import hkmc2.syntax.Keyword
import hkmc2.syntax.Tree.*

/** Infer the type of a term. */
def infer(ctx: Context, term: Term): Result[Type] =
  term match
    // Ignore the statements of MLScript block and only type the final term.

    case Blk(_, term) =>
      infer(ctx, term)

    // Type MLScript literals using type variables.

    case UnitVal() | Lit(UnitLit(_)) =>
      Ok(TVar("Unit"), Nil)
    case Lit(BoolLit(_)) =>
      Ok(TVar("Bool"), Nil)
    case Lit(IntLit(_)) =>
      Ok(TVar("Int"), Nil)
    case Lit(DecLit(_)) =>
      Ok(TVar("Decimal"), Nil)
    case Lit(StrLit(_)) =>
      Ok(TVar("String"), Nil)

    // Variable

    case Ref(symbol) =>
      val varName = symbol.nme
      Ok(ctx.getVarType(varName), Nil)

    // Lambda abstraction

    case Lam(params, body) =>
      val paramsCount = params.paramCountLB
      if paramsCount != 1 then
        throw new Exception(s"Only functions with a single parameter are supported, ${paramsCount} found.")

      var paramName = params.allParams(0).sym.name

      ctx.withFreshVar((varName, ctx) =>
        val var_ = TVar(varName)
        ctx.withVar(paramName, var_, (ctx) =>
          val bodyRes = infer(ctx, body)
          if bodyRes.isFail then
            return Fail

          val (bodyType, bodyBounds) = bodyRes.ok
          Ok(TFun(var_, bodyType), bodyBounds)
        )
      )

    // Lambda application

    case Term.App(fun, arg) =>
      ctx.withFreshVar((mockRetVarName, mockRetCtx) =>
        val funRes = infer(mockRetCtx, fun)
        if funRes.isFail then
          return Fail

        val (funType, funBounds) = funRes.ok
        val argRes = infer(mockRetCtx, arg)
        if argRes.isFail then
          return Fail

        val (argType, argBounds) = funRes.ok
        val mockFunType = TFun(argType, TVar(mockRetVarName))

        val inferRes = inferConstrainSub(mockRetCtx, funType, mockFunType)
        if inferRes.isFail then
          return Fail

        val (_, inferBounds) = inferRes.ok
        Ok(TVar(mockRetVarName), inferBounds ++ argBounds ++ funBounds)
      )

    // Type ascription

    case Term.Asc(term, typeNode) =>
      val type_ = typeNode.toType()
      val inferRes = infer(ctx, term)
      if inferRes.isFail then
        return Fail

      val (inferType, inferBounds) = inferRes.ok
      val constrainRes = inferConstrainSub(ctx, inferType, type_)
      if constrainRes.isFail then
        return Fail

      val (_, constrainBounds) = constrainRes.ok
      Ok(type_, constrainBounds ++ inferBounds)

    // Match expression

    case Term.IfLike(Keyword.`if`, _) =>
      // TODO: How to desugar a match expression ?
      Ok(TBot, Nil)

    case _ =>
      throw new Exception(s"Unsupported term: ${term.toString()}")

/** Constrain a type to be a subtype of another in type inference. */
def inferConstrainSub(ctx: Context, sub: Type, sup: Type): Result[Unit] =
  given Context = ctx
  given Mode = Mode.Constrain
  constrainSub(sub, sup)
