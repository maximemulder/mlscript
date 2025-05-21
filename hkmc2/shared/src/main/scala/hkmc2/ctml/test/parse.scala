package hkmc2.ctml.test

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.semantics.BlockMemberSymbol
import hkmc2.semantics.Branch
import hkmc2.semantics.ClassDef
import hkmc2.semantics.Fld
import hkmc2.semantics.Import
import hkmc2.semantics.Pattern
import hkmc2.semantics.Split
import hkmc2.semantics.Statement
import hkmc2.semantics.Term
import hkmc2.semantics.TermDefinition
import hkmc2.syntax.Tree

/** Convert a MLScript block to CTML statements. */
def parseStmts(mlStmts: Term): List[Stmt] =
  mlStmts match
    case Term.Blk(mlStmts, mlStmt) =>
      val stmts = mlStmts.flatMap(parseDeclStmt)
      val stmt  = parseStmt(mlStmt)
      stmts ::: List(stmt)
    case _ =>
      parseStmt(mlStmts) :: Nil

/** Convert a MLScript statement to a CTML declaration statement. */
def parseDeclStmt(mlStmt: Statement): Option[Stmt] =
  mlStmt match
    case Import(_, _) =>
      None
    case ClassDef.Plain(_, _, _, mlSymbol,_, _, _, _, _) =>
      Some(parseTypeDecl(mlSymbol))
    case TermDefinition(_, _, mlSymbol, _, _, mlType, _, _, _, _, _) =>
      Some(parseExprDecl(mlSymbol, mlType))
    case _ =>
      throw new ParseError(mlStmt)

/** Convert a MLScript symbol to a CTML type declaration. */
def parseTypeDecl(mlSymbol: BlockMemberSymbol) =
  val name = mlSymbol.nme
  StmtTypeDecl(name)

/** Convert a MLScript symbol and type to a CTML variable declaration. */
def parseExprDecl(mlSymbol: BlockMemberSymbol, mlType: Option[Term]) =
  val name = mlSymbol.nme
  val type_ = mlType match
    case Some(mlType) =>
      parseType(mlType)
    case None =>
      TTop

  StmtExprDecl(name, type_)

/** Convert a MLScript term to a CTML statement. */
def parseStmt(mlStmt: Term): Stmt =
  mlStmt match
    case Term.App(Term.Ref(mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.nme == "==" =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      StmtTypeRel(TypeRel.Eq, left, right)
    case Term.App(Term.Ref(mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.nme == "!=" =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      StmtTypeRel(TypeRel.Ne, left, right)
    case Term.App(Term.Ref(mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.nme == "<=" =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      StmtTypeRel(TypeRel.Sub, left, right)
    case Term.App(Term.Ref(mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.nme == ">=" =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      StmtTypeRel(TypeRel.Sup, left, right)
    case _ =>
      StmtExpr(parseExpr(mlStmt))

/** Convert a MLScript term to a CTML type. */
def parseType(mlType: Term): Type =
  mlType match
    case Term.Tup(mlElems) =>
      if mlElems.length != 1 then
        throw ParseError(mlType)

      val mlElem = mlElems(0)
      if mlElem.subTerms.length != 1 then
        throw ParseError(mlType)

      val mlTerm = mlElem.subTerms(0)
      parseType(mlTerm)
    case Term.Ref(mlSymbol) =>
      mlSymbol.nme match
        case "Top" =>
          TTop
        case "Bot" =>
          TBot
        case name =>
          TVar(name)
    case Term.FunTy(mlParam, mlRet, _) =>
      val param = parseType(mlParam)
      val ret   = parseType(mlRet)
      TLam(param, ret)
    case Term.CompType(mlLeft, mlRight, true) =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TUnion(left, right)
    case Term.CompType(mlLeft, mlRight, false) =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TUnion(left, right)
    case _ =>
      throw ParseError(mlType)

/** Convert a MLScript term to a CTML expression. */
def parseExpr(mlExpr: Term): Expr =
  mlExpr match
    // Only parse the final term of blocks.
    case Term.Blk(_, mlExpr) =>
      parseExpr(mlExpr)
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
    // Parse lambda abstractions.
    case Term.Lam(mlParams, mlBody) =>
      val paramsCount = mlParams.paramCountLB
      if paramsCount != 1 then
        throw new ParseError(mlExpr)

      val param = mlParams.allParams(0).sym.name
      val body  = parseExpr(mlBody)
      ELam(param, body)
    // Parse lambda applications.
    case Term.App(mlLambda, mlArg) =>
      val lam = parseExpr(mlLambda)
      val arg = parseExpr(mlArg)
      EApp(lam, arg)
    // Parse type ascriptions.
    case Term.Asc(mlExpr, mlType) =>
      val expr  = parseExpr(mlExpr)
      val type_ = parseType(mlType)
      EAscr(expr, type_)
    // Parse pattern matching.
    case Term.IfLike(_, Split.Let(_, mlScrutinee, mlCases)) =>
      val expr  = parseExpr(mlScrutinee)
      val cases = parseCases(mlCases)
      EMatch(expr, cases)
    case Term.IfLike(_, mlMatch) =>
      parseMatch(mlMatch)
    case _ =>
      throw new ParseError(mlExpr)

/** Convert an MLScript split to a CTML pattern matching expression */
def parseMatch(mlMatch: Split): EMatch =
  mlMatch match
    case Split.Let(_, mlScrutinee, mlCases) =>
      val scrutinee = parseExpr(mlScrutinee)
      val cases     = parseCases(mlCases)
      EMatch(scrutinee, cases)
    case Split.Cons(mlCase @ Branch(mlScrutinee, _, _), mlCases) =>
      val scrutinee = parseExpr(mlScrutinee)
      val case_     = parseCase(mlCase)
      val cases     = parseCases(mlCases)
      EMatch(scrutinee, case_ :: cases)
    case _ =>
      throw new ParseError(Term.Error)

/** Convert an MLScript split to a list of CTML pattern matching cases. */
def parseCases(mlCases: Split): List[EMatchCase] =
  mlCases match
    case Split.End =>
      Nil
    case Split.Cons(mlCase, mlCases) =>
      val case_ = parseCase(mlCase)
      val cases = parseCases(mlCases)
      case_ :: cases
    case _ =>
      throw new ParseError(Term.Error)

/** Convert an MLScript branch to a CTML pattern matching case. */
def parseCase(mlCase: Branch): EMatchCase =
  mlCase match
    case Branch(_, Pattern.ClassLike(_, mlPattern, _, _), Split.Else(mlBody)) =>
      val pattern = parseType(mlPattern)
      val body    = parseExpr(mlBody)
      EMatchCase(pattern, body)
    case _ =>
      throw new ParseError(Term.Error)
