package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.semantics.Term
import hkmc2.syntax.Tree

extension (term: Term)
  /** Convert a MLScript term to a CTML expression, or throw an exception if that term is not a
   * valid CTML expression. */
  def parseExpr(): Expr =
    term match
      // Only parse the final term of blocks.
      case Term.Blk(_, term) =>
        term.parseExpr()
      // Parse literals as variables.
      case Term.UnitVal() | Term.Lit(Tree.UnitLit(_)) =>
        EVar("Unit")
      case Term.Lit(Tree.BoolLit(_)) =>
        EVar("Bool")
      case Term.Lit(Tree.IntLit(_)) =>
        EVar("Int")
      case Term.Lit(Tree.DecLit(_)) =>
        EVar("Decimal")
      case Term.Lit(Tree.StrLit(_)) =>
        EVar("String")
      // Parse variables.
      case Term.Ref(symbol) =>
        val varName = symbol.nme
        EVar(varName)
      // Parse lambda abstractions.
      case Term.Lam(paramsTerm, bodyTerm) =>
        val paramsCount = paramsTerm.paramCountLB
        if paramsCount != 1 then
          throw new ParseError(term)

        val param = paramsTerm.allParams(0).sym.name
        val body  = bodyTerm.parseExpr()
        ELam(param, body)
      // Parse lambda applications.
      case Term.App(lamTerm, argTerm) =>
        val lam = lamTerm.parseExpr()
        val arg = argTerm.parseExpr()
        EApp(lam, arg)
      // Parse type ascriptions.
      case Term.Asc(exprTerm, typeTerm) =>
        val expr = exprTerm.parseExpr()
        val type_ = typeTerm.parseType()
        EAscr(expr, type_)
      case _ =>
          throw new ParseError(term)

  /** Convert a MLScript term to a CTML type, or throw an exception if that term is not a valid
   * CTML type. */
  def parseType(): Type =
    term match
      case Term.Tup(elems) =>
        if elems.length != 1 then
          throw ParseError(term)

        val elem = elems(0)
        if elem.subTerms.length != 1 then
          throw ParseError(term)

        elem.subTerms(0).parseType()
      case Term.Ref(symbol) =>
        TVar(symbol.nme)
      case Term.FunTy(param, ret, _) =>
        TFun(param.parseType(), ret.parseType())
      case _ =>
        throw ParseError(term)
