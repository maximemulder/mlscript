package hkmc2.ctml.parser

import hkmc2.asInt

import hkmc2.ctml.test.*
import hkmc2.ctml.types.*
import hkmc2.semantics.Annot
import hkmc2.semantics.BlockMemberSymbol
import hkmc2.semantics.DefineVar
import hkmc2.semantics.LetDecl
import hkmc2.semantics.ClassDef
import hkmc2.semantics.Fld
import hkmc2.semantics.Import
import hkmc2.semantics.ModuleOrObjectDef
import hkmc2.semantics.Param
import hkmc2.semantics.ParamList
import hkmc2.semantics.Statement
import hkmc2.semantics.Term
import hkmc2.semantics.TermDefinition
import hkmc2.semantics.TypeAliasSymbol
import hkmc2.semantics.TypeDef
import hkmc2.syntax.Keyword
import hkmc2.syntax.Tree

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
      case Term.Lit(Tree.UnitLit(false)) | Import(_, _, _) | LetDecl(_, _) =>
        return None
      case definition: ClassDef.Parameterized if !isAbstract(definition.annotations) =>
        parseConstructorDecl(definition)
      case ClassDef.Plain(_, _, _, mlSymbol,_, mlParent, _, _, mlAnnotations, _, _) if !isAbstract(mlAnnotations) =>
        parseClassDecl(mlSymbol, mlParent)
      case ModuleOrObjectDef(_, _, mlSymbol,_, _, _, mlParent, _, _, _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseClassDecl(mlSymbol, mlParent)
      case TypeDef(mlSymbol, _, _, None, _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseRigidVarDecl(mlSymbol)
      case TypeDef(mlSymbol, _, _, None, _, mlAnnotations) if isAbstract(mlAnnotations) =>
        parseFlexVarDecl(mlSymbol)
      case TypeDef(mlSymbol, _, _, Some(mlType), _, mlAnnotations) if !isAbstract(mlAnnotations) =>
        parseTypeVar(mlSymbol, mlType)
      case TermDefinition(_, mlSymbol, _, mlParams, mlTypeParams, mlType, None, _, _, _, _) =>
        parseExprDecl(mlSymbol, mlParams, mlTypeParams.getOrElse(Nil), mlType)
      case definition: TermDefinition if definition.body.isDefined =>
        val (variable, expr) = parseBinding(definition)
        StmtExprVar(variable, expr)
      case definition: DefineVar =>
        val (variable, expr) = parseBinding(definition)
        StmtExprVar(variable, expr)
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
      Some(ClassVar(mlParentSymbol.nme))
    case Some(Term.New(Term.TyApp(Term.Ref(mlParentSymbol), _), _, _)) =>
      Some(ClassVar(mlParentSymbol.nme))
    case None =>
      None
    case Some(mlParent) =>
      throw new ParseError(mlParent)

  StmtClassDecl(name, parent, None)

/** Translate a plain data constructor. Class names denote tags; payload precision is structural.
 *  Bodies, type parameters and inheritance require additional semantics and are unsupported here. */
def parseConstructorDecl(definition: ClassDef.Parameterized)(using Scope): Stmt =
  if definition.tparams.nonEmpty || definition.ext.nonEmpty || definition.auxParams.nonEmpty ||
      !hasOnlyConstructorFields(definition) then
    throw ParseError(definition, Some(
      "CTML constructors require no type parameters, parent, auxiliary parameters, or body."))
  val fields = functionParams(definition.params).map: param =>
    if param.flags.mut || !param.flags.isVal then
      throw ParseError(definition, Some("CTML constructor parameters must be immutable val fields."))
    LambdaParam(termVar(param.sym), param.sign.map(parseType))
  StmtClassDecl(definition.bsym.nme, None,
    Some(ConstructorDecl(termVar(definition.bsym), fields)))

/** The elaborator adds one field definition per public constructor parameter to an empty body.
 *  Accept exactly those definitions, so user initializers and methods are never silently erased. */
private def hasOnlyConstructorFields(definition: ClassDef.Parameterized): Boolean =
  val params = definition.params.params
  val stats = definition.body.blk.stats
  stats.size == params.size && stats.zip(params).forall:
    case (field: TermDefinition, param) =>
      param.fldSym.contains(field.sym) && (field.body match
        case Some(Term.Ref(symbol)) => symbol == param.sym
        case _ => false)
    case _ => false

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
def parseExprDecl(mlSymbol: BlockMemberSymbol, mlParams: List[ParamList], mlTypeParams: List[Param], mlType: Option[Term])(using scope: Scope): Stmt =
  val variable = termVar(mlSymbol)
  val typeParamNames = mlTypeParams.map(_.sym.nme)
  val typeParamScope = typeParamNames.foldLeft(scope)((scope, name) => scope.withType(name))
  given Scope = typeParamScope
  val result = mlType.map(parseType(_)).getOrElse(TTop)
  val function = mlParams.foldRight(result): (params, ret) =>
    functionParams(params) match
      case Nil => TLam(TClass(ClassVar("Unit")), ret)
      case params => params.foldRight(ret): (param, ret) =>
        TLam(param.sign.map(parseType(_)).getOrElse(TTop), ret)
  val type_ = mlTypeParams.foldRight(function): (param, body) =>
    val variable = new TypeVar(param.sym.nme,
      VarProvenance(VarOrigin.Source(param.sym.uid.asInt), VarRole.Root, Set.empty))
    TUniv(variable, body)

  StmtExprDecl(variable, type_)

/** Update the parser scope with on a statement. */
def updateScope(stmt: Stmt, scope: Scope) =
  getStmtDecl(stmt) match
    case Some((name, DeclKind.Class(constructor))) =>
      scope.withClass(name, constructor)
    case Some((name, DeclKind.Type)) =>
      scope.withType(name)
    case None =>
      scope

/** Get a class or type variable declaration from a statement, if it is one. */
def getStmtDecl(stmt: Stmt): Option[(String, DeclKind)] =
  stmt match
    case StmtClassDecl(name, _, constructor) =>
      Some((name, DeclKind.Class(constructor)))
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
