package hkmc2.ctml.parser

import hkmc2.ctml.test.*
import hkmc2.ctml.types.*
import hkmc2.semantics.Annot
import hkmc2.semantics.BlockMemberSymbol
import hkmc2.semantics.ClassDef
import hkmc2.semantics.Fld
import hkmc2.semantics.Import
import hkmc2.semantics.ModuleOrObjectDef
import hkmc2.semantics.Param
import hkmc2.semantics.Statement
import hkmc2.semantics.Symbol
import hkmc2.semantics.Term
import hkmc2.semantics.TermDefinition
import hkmc2.semantics.TypeAliasSymbol
import hkmc2.semantics.TypeDef
import hkmc2.syntax.Keyword
import hkmc2.syntax.Tree
import hkmc2.ctml.config.output

/** Convert an MLScript block to CTML statements. */
def parseTermStmts(mlStmts: Term)(using Scope): List[Stmt] =
  mlStmts match
    case Term.Blk(mlStmts, mlStmt) =>
      parseStmts(mlStmts :+ mlStmt)
    case _ =>
      parseStmt(mlStmts).toList

/** Convert an MLScript statement to a CTML statement. */
def parseStmts(mlStmts: List[Statement])(using scope: Scope): List[Stmt] =
  mlStmts match
    case mlStmt :: mlStmts =>
      val stmt = parseStmt(mlStmt)
      val newScope = stmt match
        case Some(stmt) =>
          updateScope(stmt, scope)
        case None =>
          scope
      stmt.toList ++ parseStmts(mlStmts)(using newScope)
    case Nil =>
      Nil

/** Convert an MLScript statement to a CTML statement. */
def parseStmt(mlStmt: Statement)(using Scope): Option[Stmt] =
  Some(
    mlStmt match
      case Term.Lit(Tree.UnitLit(false)) | Import(_, _, _) =>
        return None
      case ClassDef.Plain(_, _, _, mlSymbol,_, mlParent, _, _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseClassDecl(mlSymbol, mlParent)
      case ModuleOrObjectDef(_, _, mlSymbol,_, _, _, mlParent, _, _, _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseClassDecl(mlSymbol, mlParent)
      case TypeDef(mlSymbol, _, _, None, _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseRigidVarDecl(mlSymbol)
      case TypeDef(mlSymbol, _, _, None, _, mlAnnotations) if isAbstract(mlAnnotations) =>
        parseFlexVarDecl(mlSymbol)
      case TypeDef(mlSymbol, _, _, Some(mlType), _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseTypeVar(mlSymbol, mlType)
      case TermDefinition(_, mlSymbol, _, _, _, mlType, None, _, _, _, _) =>
        parseExprDecl(mlSymbol, mlType)
      case TermDefinition(_, mlSymbol, _, mlParams, _, mlType, Some(mlExpr), _, _, _, _) =>
        parseExprVar(mlSymbol, mlParams.map(_.allParams).flatten.toList, mlType, mlExpr)
      case Term.App(Term.SynthSel(_, mlIdent), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlIdent.name == "equals" =>
        val left  = parseType(mlLeft)
        val right = parseType(mlRight)
        StmtTypeRel(TypeRel.Eq, left, right)
      case Term.App(Term.Ref(mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.nme == "!=" =>
        val left  = parseType(mlLeft)
        val right = parseType(mlRight)
        StmtTypeRel(TypeRel.Ne, left, right)
      case Term.App(Term.SynthSel(_, mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.name == "Sub" =>
        val left  = parseType(mlLeft)
        val right = parseType(mlRight)
        StmtTypeRel(TypeRel.Sub, left, right)
      case Term.App(Term.SynthSel(_, mlSymbol), Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _)))) if mlSymbol.name == "Sup" =>
        val left  = parseType(mlLeft)
        val right = parseType(mlRight)
        StmtTypeRel(TypeRel.Sup, left, right)
      case mlTerm: Term =>
        StmtExpr(parseExpr(mlTerm))
      case _ =>
        throw new ParseError(mlStmt)
  )

/** Convert an MLScript class declaration to a CTML class declaration. */
def parseClassDecl(mlSymbol: BlockMemberSymbol, mlParent: Option[Term.New])(using Scope): Stmt =
  val name = mlSymbol.nme
  val parent = mlParent match
    case Some(Term.New(Term.Ref(mlParentSymbol), _, _)) =>
      Some(mlParentSymbol.nme)
    case Some(Term.New(Term.TyApp(Term.Ref(mlParentSymbol), _), _, _)) =>
      Some(mlParentSymbol.nme)
    case None =>
      None
    case Some(mlParent) =>
      throw new ParseError(mlParent)

  StmtClassDecl(name, parent)

/** Convert an MLScript type declaration to a CTML rigid type variable declaration. */
def parseRigidVarDecl(mlSymbol: TypeAliasSymbol)(using Scope): Stmt =
  val name = mlSymbol.nme
  StmtTypeDecl(name, TypeVarKind.Rigid)

/** Convert an MLScript type declaration to a CTML flexible type variable declaration. */
def parseFlexVarDecl(mlSymbol: TypeAliasSymbol)(using Scope): Stmt =
  val name = mlSymbol.nme
  StmtTypeDecl(name, TypeVarKind.Flex)

/** Convert an MLScript type alias to a CTML type variable assignment. */
def parseTypeVar(mlSymbol: TypeAliasSymbol, mlType: Term)(using Scope): Stmt =
  val name  = mlSymbol.nme
  val type_ = parseType(mlType)
  StmtTypeVar(name, type_)

/** Convert an MLScript term declaration to a CTML expression variable declaration. */
def parseExprDecl(mlSymbol: BlockMemberSymbol, mlType: Option[Term])(using Scope): Stmt =
  val name  = mlSymbol.nme
  val type_ = mlType match
    case Some(mlType) =>
      parseType(mlType)
    case None =>
      TTop

  StmtExprDecl(name, type_)

/** Convert an MLScript term declaration to a CTML expression variable assignment. */
def parseExprVar(mlSymbol: BlockMemberSymbol, mlParams: List[Param], mlType: Option[Term], mlExpr: Term)(using Scope): Stmt =
  val name  = mlSymbol.nme
  val type_ = mlType.map(parseType(_))
  var expr = parseExprVarBody(mlParams, mlType, mlExpr)

  // If the term is recursive, wrap it in a fixed-point combinator.
  if containsSymbol(mlExpr, mlSymbol) then
    expr = wrapRecursive(name, expr)

  StmtExprVar(name, expr)

/** Convert an MLScript term declaration to a CTML expression body. */
def parseExprVarBody(mlParams: List[Param], mlType: Option[Term], mlExpr: Term)(using Scope): Expr =
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

/** Update the parser scope with on a statement. */
def updateScope(stmt: Stmt, scope: Scope) =
  getStmtDecl(stmt) match
    case Some((name, DeclKind.Class)) =>
      scope.withClass(name)
    case Some((name, DeclKind.Type)) =>
      scope.withType(name)
    case None =>
      scope

/** Get a class or type variable declaration from a statement, if it is one. */
def getStmtDecl(stmt: Stmt): Option[(String, DeclKind)] =
  stmt match
    case StmtClassDecl(name, _) =>
      Some((name, DeclKind.Class))
    case StmtTypeDecl(name, _) =>
      Some((name, DeclKind.Type))
    case StmtTypeVar(name, _) =>
      Some((name, DeclKind.Type))
    case _ =>
      None

/** Check whether a list of annotations has a `declare` keyword, which is used to differentiate
 *  flexible and rigid type variable declarations.
 */
def isAbstract(mlAnnotations: List[Annot]): Boolean =
  mlAnnotations.exists(_ match
    case Annot.Modifier(Keyword.`declare`) =>
      true
    case _ =>
      false
  )

/** Wrap a recursive term in a fixed-point combinator. */
def wrapRecursive(name: String, expr: Expr): Expr =
  EApp(EVar("fix"), ELam(name, expr))

/** Check whether a MLScript term contains a given symbol. */
def containsSymbol(mlTerm: Term, mlSymbol: Symbol): Boolean =
  mlTerm match
    case Term.Ref(mlTermSymbol) if mlTermSymbol == mlSymbol =>
      true
    case _ =>
      mlTerm.subTerms.exists(containsSymbol(_, mlSymbol))
