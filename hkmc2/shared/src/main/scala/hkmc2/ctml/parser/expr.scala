package hkmc2.ctml.parser

import hkmc2.ctml.types.*
import hkmc2.semantics.DefineVar
import hkmc2.semantics.Elem
import hkmc2.semantics.Fld
import hkmc2.semantics.LetDecl
import hkmc2.semantics.Param
import hkmc2.semantics.Pattern
import hkmc2.semantics.SimpleSplit
import hkmc2.semantics.Statement
import hkmc2.semantics.Term
import hkmc2.semantics.Spd
import hkmc2.syntax.Tree


/** Convert an MLScript term to a CTML expression. */
def parseExpr(mlExpr: Term): Expr =
  mlExpr match
    // Parse blocks.
    case Term.Blk(mlLefts, mlRight) =>
      parseBlock(mlLefts, mlRight)
    // Parse literals as variables.
    case Term.UnitVal() | Term.Lit(Tree.UnitLit(_)) =>
      EVar("unit")
    case Term.Lit(Tree.BoolLit(_)) =>
      EVar("bool")
    case Term.Lit(Tree.IntLit(_)) =>
      EVar("int")
    case Term.Lit(Tree.DecLit(_)) =>
      EVar("num")
    case Term.Lit(Tree.StrLit(_)) =>
      EVar("string")
    // Parse variables.
    case Term.Ref(mlSymbol) =>
      val name = mlSymbol.nme
      EVar(name)
    case Term.SynthSel(_, mlIdent) =>
      EVar(mlIdent.name)
    // Parse lambda abstractions.
    case Term.Lam(mlParams, mlBody) =>
      parseLambda(mlParams.allParams, mlBody)
    // Parse lambda applications.
    case Term.App(mlLambda, mlArg) =>
      parseApp(mlLambda, mlArg)
    // Parse type ascriptions.
    case Term.Asc(mlExpr, mlType) =>
      val expr  = parseExpr(mlExpr)
      val type_ = parseType(mlType)
      EAscr(expr, type_)
    case Term.IfLike(_, SimpleSplit.Cons(SimpleSplit.Head.Let(_, mlCondition), SimpleSplit.Cons(SimpleSplit.Head.Match(_, _, SimpleSplit.Else(mlThen)), SimpleSplit.Else(mlElse)))) =>
      val condition = parseExpr(mlCondition)
      val then_ = parseExpr(mlThen)
      val else_ = parseExpr(mlElse)
      EApp(EApp(EApp(EVar("_if_"), condition), then_), else_)
    case Term.IfLike(_, mlMatch) =>
      parseMatch(mlMatch)
    case _ =>
      throw new ParseError(mlExpr)

/** Parse a block, which can either be a variable binding or a tuple. */
def parseBlock(mlLefts: List[Statement], mlRight: Term): Expr =
  mlLefts match
    case Nil =>
      parseExpr(mlRight)
    case mlLeft :: mlLefts =>
      val next = parseBlock(mlLefts, mlRight)
      mlLeft match
        case DefineVar(mlSymbol, mlExpr) =>
          val name = mlSymbol.nme
          val expr = parseExpr(mlExpr)
          val lambda = ELam(name, next)
          EApp(lambda, expr)
        case LetDecl(_, _) =>
          next
        case mlLeft: Term =>
          val left = parseExpr(mlLeft)
          val right = parseBlock(mlLefts, mlRight)
          ETuple(left, right)
        case _ =>
          throw new ParseError(mlLeft)

/** Convert an MLScript lambda abstraction to a CTML expression. */
def parseLambda(mlParams: List[Param], mlBody: Term): Expr =
  mlParams match
    case Nil =>
      throw new ParseError(mlBody)
    case mlParam :: mlParams =>
      val paramName = mlParam.sym.nme
      val body = parseLambdaBody(mlParams, mlBody)
      ELam(paramName, body)

/** Convert an MLScript lambda abstraction to a CTML lambda abstraction body. */
def parseLambdaBody(mlParams: List[Param], mlBody: Term): Expr =
  mlParams match
    case Nil =>
      parseExpr(mlBody)
    case mlParam :: mlParams =>
      val paramName = mlParam.sym.nme
      val body = parseLambdaBody(mlParams, mlBody)
      ELam(paramName, body)

/** Convert an MLScript lambda application to a CTML expresssion. */
def parseApp(mlLambda: Term, mlArgs: Term): Expr =
  mlArgs match
    case Term.Tup(mlArgs :+ Fld(_, mlArg, _)) =>
      val lambda = parseAppLambda(mlLambda, mlArgs)
      val arg = parseExpr(mlArg)
      EApp(lambda, arg)
    case _ =>
      throw new ParseError(mlLambda)

/** Convert an MLScript lambda application to a CTML lambda application lambda. */
def parseAppLambda(mlLambda: Term, mlArgs: List[Elem]): Expr =
  mlArgs match
    case mlArgs :+ Fld(_, mlArg, _) =>
      val lambda = parseAppLambda(mlLambda, mlArgs)
      val arg = parseExpr(mlArg)
      EApp(lambda, arg)
    case _ =>
      mlLambda match
        case Term.SynthSel(_, mlIdent) if mlIdent.name == "equals" =>
          EVar("==")
        case _ =>
          parseExpr(mlLambda)

/** Convert an MLScript split to a CTML pattern matching expression */
def parseMatch(mlMatch: SimpleSplit): EMatch =
  // TODO: This could be prettier.
  mlMatch match
    case SimpleSplit.Cons(mlCase @ SimpleSplit.Head.Let(_, mlScrutinee), mlCases: SimpleSplit.Cons) =>
      parseCases(mlScrutinee, mlCases)
    case mlMatch @ SimpleSplit.Cons(SimpleSplit.Head.Match(mlScrutinee, _, _), _) =>
      parseCases(mlScrutinee, mlMatch)
    case _ =>
      throw new ParseError(Term.Error)

def parseCases(mlScrutinee: Term, mlMatch: SimpleSplit.Cons): EMatch =
  mlMatch.branch match
    case SimpleSplit.Head.Match(_, Pattern.Constructor(mlPattern, _), SimpleSplit.Else(mlBody)) =>
      val scrutinee = parseExpr(mlScrutinee)
      val pattern = parseType(mlPattern)
      val then_ = parseExpr(mlBody)
      val else_ = mlMatch.tail match
        case mlMatch: SimpleSplit.Cons =>
          Some(parseCases(mlScrutinee, mlMatch))
        case SimpleSplit.End =>
          None
        case _ =>
          throw new ParseError(Term.Error)
      EMatch(scrutinee, pattern, then_, else_)
    case _ =>
      throw new ParseError(Term.Error)

/** Get the underlying term of an MLScript element. */
def getElemTerm(elem: Elem): Term =
  elem match
    case Fld(_, term, _) => term
    case Spd(_, term)    => term
