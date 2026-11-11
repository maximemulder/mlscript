package hkmc2.ctml.parser

import hkmc2.ctml.config.config
import hkmc2.ctml.types.*
import hkmc2.semantics.DefineVar
import hkmc2.semantics.LetDecl
import hkmc2.semantics.TermDefinition
import hkmc2.semantics.Elem
import hkmc2.semantics.Fld
import hkmc2.semantics.Param
import hkmc2.semantics.ParamList
import hkmc2.semantics.Pattern
import hkmc2.semantics.SimpleSplit
import hkmc2.semantics.Statement
import hkmc2.semantics.Term
import hkmc2.semantics.Spd
import hkmc2.syntax.Tree
import hkmc2.semantics.IfLikeForm
import hkmc2.semantics.SimpleSplit.Head

/** Convert an MLScript term to a CTML expression. */
def parseExpr(mlExpr: Term)(using Scope): Expr =
  mlExpr match
    // Parse blocks.
    case Term.Blk(mlLefts, mlRight) =>
      parseBlock(mlLefts, mlRight)
    // Binary tuples have the same shape as CTML products.
    case Term.Tup(List(Fld(_, mlLeft, _), Fld(_, mlRight, _))) =>
      ETuple(parseExpr(mlLeft), parseExpr(mlRight))
    // Parse unit independently of term bindings.
    case Term.UnitVal() | Term.Lit(Tree.UnitLit(_)) =>
      EUnit
    // Parse other literals as variables.
    case Term.Lit(Tree.BoolLit(_)) =>
      EVar(TermVar("bool", None))
    case Term.Lit(Tree.IntLit(_)) =>
      EVar(TermVar("int", None))
    case Term.Lit(Tree.DecLit(_)) =>
      EVar(TermVar("num", None))
    case Term.Lit(Tree.StrLit(_)) =>
      EVar(TermVar("string", None))
    // Parse variables.
    case Term.Ref(mlSymbol) =>
      EVar(termVar(mlSymbol))
    case Term.SynthSel(_, mlIdent) =>
      EVar(TermVar(mlIdent.name, None))
    // Immutable constructor fields are projected structurally.
    case Term.Sel(record, field) =>
      EProj(parseExpr(record), field.name)
    case Term.New(target, List(args: Term.Tup), None) =>
      constructor(target)
      parseApp(target, args)
    // Parse lambda abstractions.
    case Term.Lam(mlParams, mlBody) =>
      parseLambda(mlParams, mlBody)
    // Parse lambda applications.
    case Term.App(mlLambda, mlArg) =>
      parseApp(mlLambda, mlArg)
    case Term.TyApp(expr, arguments) =>
      ETypeApp(parseExpr(expr), arguments.map(parseType))
    // Parse type ascriptions.
    case Term.Asc(mlExpr, mlType) =>
      val expr  = parseExpr(mlExpr)
      val type_ = parseType(mlType)
      EAscr(expr, type_)
    // MLScript parses `foo as A & B` as expression-level composition whose
    // left operand is an ascription. CTML only supports this shape as a
    // compact way to ascribe an intersection or union type.
    case Term.CompType(mlLeft, mlRight, mlPol) =>
      parseAscriptionComposition(mlLeft, mlRight, mlPol)
    // MLScript binds a scrutinee that is not a variable before matching it, so a conditional is a
    // binding matched against `true`, while a binding matched against any other pattern is a match.
    case Term.IfLike(_, IfLikeForm.ReturningIf, SimpleSplit.Cons(SimpleSplit.Head.Let(_, mlCondition), SimpleSplit.Cons(SimpleSplit.Head.Match(_, Pattern.Literal(Tree.BoolLit(true)), SimpleSplit.Else(mlThen)), SimpleSplit.Else(mlElse)))) =>
      val condition = parseExpr(mlCondition)
      val then_ = parseExpr(mlThen)
      val else_ = parseExpr(mlElse)
      EApp(EApp(EApp(EVar(TermVar("_if_", None)), condition), then_), else_)
    case Term.IfLike(_, IfLikeForm.ReturningIf, mlSplit) =>
      val body = parseSplit(mlSplit)
      mlSplit match
        case SimpleSplit.Cons(Head.Match(scrutinee, _, _), _) if config.wrapIfs =>
          // Rebinding the scrutinee gives context join a non-expansive enclosing lambda and
          // refines its occurrences throughout the split. A leading UCS let already does this.
          val variable = termVar(scrutinee.sym)
          EApp(ELam(Some(LambdaParam(variable, None)), body), EVar(variable))
        case _ =>
          body
    case _ =>
      throw new ParseError(mlExpr)

/** Convert an MLScript expression-level type composition to a CTML ascription. */
def parseAscriptionComposition(mlLeft: Term, mlRight: Term, mlPol: Boolean)(using Scope): Expr =
  val right = parseType(mlRight)
  val type_ = (left: Type) =>
    TJointType(parseJointMode(mlPol), left, right)
  mlLeft match
    case Term.Asc(mlExpr, mlType) =>
      EAscr(parseExpr(mlExpr), type_(parseType(mlType)))
    case Term.CompType(_, _, _) =>
      parseExpr(mlLeft) match
        case EAscr(expr, left) =>
          EAscr(expr, type_(left))
        case _ =>
          throw new ParseError(mlLeft)
    case _ =>
      throw new ParseError(mlLeft)

/** Parse sequential immutable bindings as lambda applications.
 *  Initializers retain their obligations even when the bound name is unused. Definitions are
 *  available only to subsequent bindings and the result; forward references are unsupported. */
def parseBlock(mlLefts: List[Statement], mlRight: Term)(using Scope): Expr =
  mlLefts match
    case Nil => parseExpr(mlRight)
    case LetDecl(_, _) :: rest => parseBlock(rest, mlRight)
    case (head: DefineVar) :: rest => parseLocalBinding(head, rest, mlRight)
    case (head: TermDefinition) :: rest if head.body.isDefined =>
      parseLocalBinding(head, rest, mlRight)
    case head :: _ => throw ParseError(head)

/** Apply the continuation of one local binding to its initializer. */
private def parseLocalBinding(stmt: Statement, rest: List[Statement], result: Term)
  (using Scope): Expr =
  val (variable, expr) = parseBinding(stmt)
  EApp(ELam(Some(LambdaParam(variable, None)), parseBlock(rest, result)), expr)

/** Convert an MLScript lambda abstraction to a CTML expression. */
def parseLambda(mlParams: ParamList, mlBody: Term)(using Scope): Expr =
  wrapLambda(mlParams, parseExpr(mlBody))

/** Wrap a body in one parameter list, retaining an empty list as a Unit-domain abstraction. */
def wrapLambda(mlParams: ParamList, body: Expr)(using Scope): Expr =
  functionParams(mlParams) match
    case Nil =>
      ELam(None, body)
    case params =>
      params.foldRight(body): (param, body) =>
        ELam(Some(LambdaParam(termVar(param.sym), param.sign.map(parseType))), body)

/** Validate and unpack an ordinary parameter list, including an empty one. */
def functionParams(params: ParamList): List[Param] =
  if params.restParam.isDefined || params.flags.ctx then
    throw ParseError(Term.Lam(params, Term.UnitVal()),
      Some("Rest and contextual parameters are unsupported in CTML."))
  params.params

/** Convert an MLScript lambda application to a CTML expresssion. */
def parseApp(mlLambda: Term, mlArgs: Term)(using scope: Scope): Expr =
  (mlLambda, mlArgs) match
    case (Term.Ref(symbol), Term.Tup(args)) =>
      scope.get(symbol.nme) match
        case Some(DeclKind.Class(Some(ctor))) if ctor.variable == termVar(symbol) =>
          if args.size != ctor.fields.size then
            throw ParseError(mlArgs, Some("Incorrect constructor arity."))
        case _ =>
    case _ =>
  mlArgs match
    case Term.Tup(Nil) =>
      EApp(parseExpr(mlLambda), EUnit)
    case Term.Tup(mlArgs :+ Fld(_, mlArg, _)) =>
      val lambda = parseAppLambda(mlLambda, mlArgs)
      val arg = parseExpr(mlArg)
      EApp(lambda, arg)
    case _ =>
      throw new ParseError(mlLambda)

/** Convert an MLScript lambda application to a CTML lambda application lambda. */
def parseAppLambda(mlLambda: Term, mlArgs: List[Elem])(using Scope): Expr =
  mlArgs match
    case mlArgs :+ Fld(_, mlArg, _) =>
      val lambda = parseAppLambda(mlLambda, mlArgs)
      val arg = parseExpr(mlArg)
      EApp(lambda, arg)
    case Nil =>
      mlLambda match
        case Term.SynthSel(_, mlIdent) if mlIdent.name == "equals" =>
          EVar(TermVar("==", None))
        case _ =>
          parseExpr(mlLambda)
    case _ => throw ParseError(mlLambda)

/** Convert an MLScript split to a CTML expression */
def parseSplit(mlSplit: SimpleSplit)(using Scope): Expr =
  mlSplit match
    case mlCons: SimpleSplit.Cons =>
      parseCons(mlCons)
    case SimpleSplit.Else(mlExpr) =>
      parseExpr(mlExpr)
    case SimpleSplit.End =>
      throw new ParseError(Term.Error())

/** Convert an MLScript cons to a CTML expression */
def parseCons(mlCons: SimpleSplit.Cons)(using Scope): Expr =
  mlCons match
    case SimpleSplit.Cons(mlLet @ SimpleSplit.Head.Let(_, _), mlTail: SimpleSplit.Cons) =>
      parseLet(mlLet, mlTail)
    case SimpleSplit.Cons(mlMatch : SimpleSplit.Head.Match, mlTail) =>
      parseMatch(mlMatch, mlTail)
    case _ =>
      throw new ParseError(Term.Error())

/** Convert an MLScript let binding to a CTML expression */
def parseLet(mlLet: SimpleSplit.Head.Let, mlTail: SimpleSplit.Cons)(using Scope): Expr =
  val param = termVar(mlLet.binding)
  val arg = parseExpr(mlLet.term)
  val body = parseCons(mlTail)
  val direct = if config.dewrapUcsIfs then dewrapMatchBinding(param, arg, body) else None
  direct.getOrElse(EApp(ELam(Some(LambdaParam(param, None)), body), arg))

/** Convert an MLScript pattern match to a CTML expression */
def parseMatch(mlMatch: SimpleSplit.Head.Match, mlTail: SimpleSplit)(using Scope): EMatch =
  val scrutinee = parseExpr(mlMatch.scrutinee)
  val (pattern, bindings) = parsePattern(mlMatch.pattern, scrutinee)
  val then_ = bindings.foldRight(parseSplit(mlMatch.consequent)):
    case ((variable, value), body) => EApp(ELam(Some(LambdaParam(variable, None)), body), value)
  val else_ = mlTail match
    case mlCons: SimpleSplit.Cons =>
      Some(parseCons(mlCons))
    case SimpleSplit.Else(mlElse) =>
      Some(parseExpr(mlElse))
    case SimpleSplit.End =>
      None
  EMatch(scrutinee, pattern, then_, else_)

/** Look up the metadata of a supported constructor without confusing shadowing term bindings. */
private def constructor(target: Term)(using scope: Scope): ConstructorDecl =
  target match
    case Term.Ref(symbol) =>
      scope.get(symbol.nme) match
        case Some(DeclKind.Class(Some(ctor))) if ctor.variable == termVar(symbol) => ctor
        case _ => throw ParseError(target, Some("Unsupported CTML constructor."))
    case _ => throw ParseError(target, Some("Unsupported CTML constructor."))

/** Translate a pattern into a runtime-testable type and bindings for its successful branch.
 *  Nested field tests stay in the pattern type, so failure negates the entire pattern and preserves
 *  fallthrough. Projections are evaluated only after all its tests succeed. */
def parsePattern(mlPattern: Pattern, scrutinee: Expr)(using Scope)
  : (Type, List[(TermVar, Expr)]) =
  mlPattern match
    case Pattern.Wildcard() => (TTop, Nil)
    case Pattern.Tuple(List(left, right), None) =>
      val (leftType, leftBindings) = parsePattern(left, ETupleProj(scrutinee, isFirst = true))
      val (rightType, rightBindings) = parsePattern(right, ETupleProj(scrutinee, isFirst = false))
      (TTuple(leftType, rightType), leftBindings ++ rightBindings)
    case Pattern.Tuple(_, _) =>
      throw ParseError(Term.Error(),
        Some("CTML tuple patterns require exactly two components and no spread."))
    case alias: Pattern.Alias =>
      val (pattern, bindings) = parsePattern(alias.pattern, scrutinee)
      (pattern, (termVar(alias.symbol), scrutinee) :: bindings)
    case Pattern.Constructor(target, None) => (parseType(target), Nil)
    case Pattern.Constructor(target, Some(args)) =>
      val ctor = constructor(target)
      if args.size != ctor.fields.size then
        throw ParseError(target, Some("Incorrect constructor pattern arity."))
      ctor.fields.zip(args).foldLeft((TClass(ClassVar(ctor.variable.name)): Type,
          List.empty[(TermVar, Expr)])):
        case ((pattern, bindings), (field, arg)) =>
          val name = field.variable.name
          val (nested, nestedBindings) = parsePattern(arg, EProj(scrutinee, name))
          (TJointType(JointMode.Inter, pattern, TField(name, nested)), bindings ++ nestedBindings)
    case Pattern.Negation(inner) =>
      val (pattern, bindings) = parsePattern(inner, scrutinee)
      if bindings.nonEmpty then
        throw ParseError(Term.Error(), Some("Bindings under negated patterns are unsupported."))
      (TNeg(pattern), Nil)
    case Pattern.Composition(pol, left, right) =>
      val (leftType, leftBindings) = parsePattern(left, scrutinee)
      val (rightType, rightBindings) = parsePattern(right, scrutinee)
      if pol && (leftBindings.nonEmpty || rightBindings.nonEmpty) then
        throw ParseError(Term.Error(), Some("Bindings in alternative patterns are unsupported."))
      (TJointType(parseJointMode(pol), leftType, rightType), leftBindings ++ rightBindings)
    case _ => throw ParseError(Term.Error(), Some("Unsupported CTML pattern."))

/** Get the underlying term of an MLScript element. */
def getElemTerm(elem: Elem): Term =
  elem match
    case Fld(_, term, _) => term
    case Spd(_, term)    => term
