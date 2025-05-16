package hkmc2.ctml.core

import hkmc2.ctml.types.*
import hkmc2.semantics.Branch
import hkmc2.semantics.Pattern
import hkmc2.semantics.Split
import hkmc2.semantics.Term
import hkmc2.syntax.Tree

extension (term: Term)
  /** Convert a MLScript term to a CTML type. */
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
        TLam(param.parseType(), ret.parseType())
      case Term.CompType(left, right, true) =>
        TUnion(left.parseType(), right.parseType())
      case Term.CompType(left, right, false) =>
        TInter(left.parseType(), right.parseType())
      case _ =>
        throw ParseError(term)

  /** Convert a MLScript term to a CTML expression. */
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
      // Parse pattern matching.
      case Term.IfLike(_, Split.Let(_, exprTerm, casesTerm)) =>
        val expr = exprTerm.parseExpr()
        val cases = casesTerm.parseCases()
        EMatch(expr, cases)
      case Term.IfLike(_, split) =>
        split.parseMatch()
      case _ =>
          throw new ParseError(term)

extension (split: Split)
  /** Convert an MLScript split to a CTML pattern matching expression */
  def parseMatch(): EMatch =
    split match
      case Split.Let(_, exprTerm, casesTerm) =>
        val expr = exprTerm.parseExpr()
        val cases = casesTerm.parseCases()
        EMatch(expr, cases)
      case Split.Cons(branch @ Branch(exprTerm, _, _), consTerm) =>
        val expr = exprTerm.parseExpr()
        val case_ = branch.parseCase()
        val cases = consTerm.parseCases()
        EMatch(expr, case_ :: cases)
      case _ =>
        throw new ParseError(Term.Error)

  /** Convert an MLScript split to a list of CTML pattern matching cases. */
  def parseCases(): List[EMatchCase] =
    split match
      case Split.End =>
        Nil
      case Split.Cons(Branch(_, Pattern.ClassLike(_, typeTerm, _, _), Split.Else(exprTerm)), consTerm) =>
        val type_ = typeTerm.parseType()
        val expr = exprTerm.parseExpr()
        EMatchCase(type_, expr) :: consTerm.parseCases()
      case _ =>
        throw new ParseError(Term.Error)

extension (branch: Branch)
  /** Convert an MLScript branch to a CTML pattern matching case. */
  def parseCase(): EMatchCase =
    branch match
      case Branch(_, Pattern.ClassLike(_, typeTerm, _, _), Split.Else(exprTerm)) =>
        val type_ = typeTerm.parseType()
        val expr = exprTerm.parseExpr()
        EMatchCase(type_, expr)
      case _ =>
        throw new ParseError(Term.Error)
