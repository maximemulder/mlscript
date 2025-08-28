package hkmc2.ctml.test

import hkmc2.ctml.core.*
import hkmc2.ctml.types.*
import hkmc2.semantics.BlockMemberSymbol
import hkmc2.semantics.Branch
import hkmc2.semantics.ClassDef
import hkmc2.semantics.DefineVar
import hkmc2.semantics.Elem
import hkmc2.semantics.Fld
import hkmc2.semantics.Import
import hkmc2.semantics.LetDecl
import hkmc2.semantics.Param
import hkmc2.semantics.Pattern
import hkmc2.semantics.QuantVar
import hkmc2.semantics.Spd
import hkmc2.semantics.Split
import hkmc2.semantics.Statement
import hkmc2.semantics.Term
import hkmc2.semantics.TermDefinition
import hkmc2.semantics.TypeAliasSymbol
import hkmc2.semantics.TypeDef
import hkmc2.syntax.Tree

/** Convert an MLScript block to CTML statements. */
def parseStmts(mlStmts: Term): List[Stmt] =
  mlStmts match
    case Term.Blk(mlStmts, mlStmt) =>
      val stmts = mlStmts.flatMap(parseStmt)
      val stmt  = parseStmt(mlStmt)
      stmts ++ stmt
    case _ =>
      parseStmt(mlStmts).toList

/** Convert an MLScript statement to a CTML statement. */
def parseStmt(mlStmt: Statement): Option[Stmt] =
  Some(
    mlStmt match
      case Term.Lit(Tree.UnitLit(false)) | Import(_, _) =>
        return None
      case ClassDef.Plain(_, _, _, mlSymbol,_, _, _, _, _) =>
        parseClassDecl(mlSymbol)
      case TypeDef(mlSymbol, _, None, _, _) =>
        parseTypeDecl(mlSymbol)
      case TypeDef(mlSymbol, _, Some(mlType), _, _) =>
        parseTypeVar(mlSymbol, mlType)
      case TermDefinition(_, _, mlSymbol, _, _, mlType, None, _, _, _, _) =>
        parseExprDecl(mlSymbol, mlType)
      case TermDefinition(_, _, mlSymbol, mlParams, _, mlType, Some(mlExpr), _, _, _, _) =>
        parseExprVar(mlSymbol, mlParams.map(_.allParams).flatten.toList, mlType, mlExpr)
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
      case mlTerm: Term =>
        StmtExpr(parseExpr(mlTerm))
      case _ =>
        throw new ParseError(mlStmt)
  )

/** Convert an MLScript class declaration to a CTML class declaration. */
def parseClassDecl(mlSymbol: BlockMemberSymbol): Stmt =
  val name = mlSymbol.nme
  StmtClassDecl(name)

/** Convert an MLScript type declaration to a CTML type variable declaration. */
def parseTypeDecl(mlSymbol: TypeAliasSymbol): Stmt =
  val name = mlSymbol.nme
  StmtTypeDecl(name)

/** Convert an MLScript type alias to a CTML type variable assignment. */
def parseTypeVar(mlSymbol: TypeAliasSymbol, mlType: Term): Stmt =
  val name  = mlSymbol.nme
  val type_ = parseType(mlType)
  StmtTypeVar(name, type_)

/** Convert an MLScript term declaration to a CTML expression variable declaration. */
def parseExprDecl(mlSymbol: BlockMemberSymbol, mlType: Option[Term]): Stmt =
  val name  = mlSymbol.nme
  val type_ = mlType match
    case Some(mlType) =>
      parseType(mlType)
    case None =>
      TTop

  StmtExprDecl(name, type_)

/** Convert an MLScript term declaration to a CTML expression variable assignment. */
def parseExprVar(mlSymbol: BlockMemberSymbol, mlParams: List[Param], mlType: Option[Term], mlExpr: Term): Stmt =
  val name  = mlSymbol.nme
  val type_ = mlType.map(parseType(_))
  val expr = parseExprVarBody(mlParams, mlType, mlExpr)
  StmtExprVar(name, expr)

/** Convert an MLScript term declaration to a CTML expression body. */
def parseExprVarBody(mlParams: List[Param], mlType: Option[Term], mlExpr: Term): Expr =
  mlParams match
    case Nil =>
      val type_ = mlType.map(parseType(_))
      val expr = parseExpr(mlExpr)
      type_ match
        case Some(type_) =>
          EAscr(expr, type_)
        case None =>
          expr
    case mlParam :: mlParams =>
      val paramName = mlParam.sym.nme
      val body = parseExprVarBody(mlParams, mlType, mlExpr)
      ELam(paramName, body)

/** Convert an MLScript term to a CTML type. */
def parseType(mlType: Term): Type =
  mlType match
    case Term.Blk(mlLefts, mlRight) =>
      parseTypeTuple(mlLefts, mlRight)
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
          TVar(TypeVar(name))
    case Term.FunTy(mlParams, mlRet, _) =>
      parseTypeLambda(mlParams, mlRet)
    case Term.TyApp(mlAbs, mlArgs) =>
      parseTypeApp(mlAbs, mlArgs)
    case Term.Forall(mlVars, _, mlBody) =>
      parseTypeUniv(mlVars, mlBody)
    case Term.CompType(mlLeft, mlRight, true) =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TUnion(left, right)
    case Term.CompType(mlLeft, mlRight, false) =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TInter(left, right)
    case _ =>
      throw ParseError(mlType)

/** Convert an MLScript block to a CTML tuple type. */
def parseTypeTuple(mlLefts: List[Statement], mlRight: Term): Type =
  mlLefts match
    case Nil =>
      parseType(mlRight)
    case mlLeft :: mlLefts =>
      mlLeft match
        case mlLeft: Term =>
          val left  = parseType(mlLeft)
          val right = parseType(mlRight)
          TTuple(left, right)
        case _ =>
          throw ParseError(mlLeft)

/** Convert an MLScript function type to a CTML type. */
def parseTypeLambda(mlParams: Term, mlRet: Term): Type =
  mlParams match
    case Term.Tup(mlParams) =>
      parseTypeLambdaParams(mlParams, mlRet)
    case _ =>
      val param = parseType(mlParams)
      val ret   = parseType(mlRet)
      TLam(param, ret)

/** Convert an MLScript multi-parameter function type to a CTML type. */
def parseTypeLambdaParams(mlParams: List[Elem], mlRet: Term): Type =
  mlParams match
    case mlParam :: mlParams =>
      val param = parseType(getElemTerm(mlParam))
      val ret   = parseTypeLambdaParams(mlParams, mlRet)
      TLam(param, ret)
    case Nil =>
      parseType(mlRet)

/** Convert an MLScript type application to a CTML type. */
def parseTypeApp(mlAbs: Term, mlArgs: List[Term]): Type =
  mlArgs match
    case mlArgs :+ mlArg =>
      val abs = parseTypeApp(mlAbs, mlArgs)
      val arg = parseType(mlArg)
      TApp(abs, arg)
    case _ =>
      parseType(mlAbs)

/** Convert an MLScript universal type to a CTML type. */
def parseTypeUniv(mlVars: List[QuantVar], mlBody: Term): Type =
  mlVars match
    case mlVar :: mlVars =>
      var body = parseTypeUniv(mlVars, mlBody)
      val var_ = TypeVar(mlVar.sym.name)

      mlVar.lb match
        case Some(mlBound) =>
          val bound = parseType(mlBound)
          body = TConstrained(body, List(Bound(var_, Direction.Super, bound)))
        case None =>
          ()

      mlVar.ub match
        case Some(mlBound) =>
          val bound = parseType(mlBound)
          body = TConstrained(body, List(Bound(var_, Direction.Sub, bound)))
        case None =>
          ()

      TUniv(var_, body)
    case Nil =>
      parseType(mlBody)

/** Convert an MLScript term to a CTML expression. */
def parseExpr(mlExpr: Term): Expr =
  mlExpr match
    // Parse blocks.
    case Term.Blk(mlLefts, mlRight) =>
      parseBlock(mlLefts, mlRight)
    // Parse literals as variables.
    case Term.UnitVal() | Term.Lit(Tree.UnitLit(_)) =>
      EVar("unit")
    case Term.Lit(Tree.BoolLit(true)) =>
      EVar("true_")
    case Term.Lit(Tree.BoolLit(false)) =>
      EVar("false_")
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
    case Term.IfLike(_, Split.Let(_, mlCondition, Split.Cons(Branch(_, _, Split.Else(mlThen)), Split.Else(mlElse)))) =>
      val condition = parseExpr(mlCondition)
      val then_ = parseExpr(mlThen)
      val else_ = parseExpr(mlElse)
      EApp(EApp(EApp(EVar("if_"), condition), then_), else_)
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
    parseExpr(mlLambda)

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

/** Get the underlying term of an MLScript element. */
def getElemTerm(elem: Elem): Term =
  elem match
    case Fld(_, term, _) => term
    case Spd(_, term)    => term
