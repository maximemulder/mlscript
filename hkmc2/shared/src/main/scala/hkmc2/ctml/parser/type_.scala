package hkmc2.ctml.parser

import hkmc2.ctml.types.*
import hkmc2.semantics.Elem
import hkmc2.semantics.Term
import hkmc2.semantics.QuantVar
import hkmc2.semantics.SubConstraint
import hkmc2.semantics.SubDir
import hkmc2.ctml.config.output

/** Convert an MLScript term to a CTML type. */
def parseType(mlType: Term)(using scope: Scope): Type =
  mlType match
    case Term.Neg(mlBody) =>
      TNeg(parseType(mlBody))
    case Term.Tup(mlElems) =>
      parseTypeTuple(mlElems.init.map(_.subTerms(0)), mlElems.last.subTerms(0))
    case Term.Ref(mlSymbol) =>
      mlSymbol.nme match
        case "Top" =>
          TTop
        case "Bot" =>
          TBot
        case name =>
          scope.get(name) match
            case Some(DeclKind.Class) =>
              TClass(name)
            case Some(DeclKind.Type) =>
              TVar(TypeVar(name))
            case None =>
              // output(scope.toString)
              throw ParseError(mlType)
    case Term.FunTy(mlParams, mlRet, _) =>
      parseTypeLambda(mlParams, mlRet)
    case Term.TyApp(mlAbs, mlArgs) =>
      parseTypeApp(mlAbs, mlArgs)
    case Term.Forall(mlVars, _, mlBody) =>
      parseTypeUniv(mlVars, mlBody)
    case Term.Constrained(mlConstraints, mlBody) =>
      parseTypeConstrained(mlConstraints, mlBody)
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
def parseTypeTuple(mlLefts: List[Term], mlRight: Term)(using Scope): Type =
  mlLefts match
    case Nil =>
      parseType(mlRight)
    case mlLeft :: mlLefts =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TTuple(left, right)

/** Convert an MLScript function type to a CTML type. */
def parseTypeLambda(mlParams: Term, mlRet: Term)(using Scope): Type =
  mlParams match
    case Term.Tup(mlParams) =>
      parseTypeLambdaParams(mlParams, mlRet)
    case _ =>
      val param = parseType(mlParams)
      val ret   = parseType(mlRet)
      TLam(param, ret)

/** Convert an MLScript multi-parameter function type to a CTML type. */
def parseTypeLambdaParams(mlParams: List[Elem], mlRet: Term)(using Scope): Type =
  mlParams match
    case mlParam :: mlParams =>
      val param = parseType(getElemTerm(mlParam))
      val ret   = parseTypeLambdaParams(mlParams, mlRet)
      TLam(param, ret)
    case Nil =>
      parseType(mlRet)

/** Convert an MLScript type application to a CTML type. */
def parseTypeApp(mlAbs: Term, mlArgs: List[Term])(using Scope): Type =
  mlArgs match
    case mlArgs :+ mlArg =>
      val abs = parseTypeApp(mlAbs, mlArgs)
      val arg = parseType(mlArg)
      TApp(abs, arg)
    case _ =>
      parseType(mlAbs)

/** Convert an MLScript universal type to a CTML type. */
def parseTypeUniv(mlVars: List[QuantVar], mlBody: Term)(using scope: Scope): Type =
  mlVars match
    case mlVar :: mlVars =>
      val var_ = TypeVar(mlVar.sym.name)
      val newScope = scope.withType(mlVar.sym.name)
      var body = parseTypeUniv(mlVars, mlBody)(using newScope)

      mlVar.lb match
        case Some(mlBound) =>
          val bound = parseType(mlBound)(using newScope)
          body = TConstrained(body, Constraint(TVar(var_), Direction.Super, bound))
        case None =>
          ()

      mlVar.ub match
        case Some(mlBound) =>
          val bound = parseType(mlBound)(using newScope)
          body = TConstrained(body, Constraint(TVar(var_), Direction.Sub, bound))
        case None =>
          ()

      TUniv(var_, body)
    case Nil =>
      parseType(mlBody)

/** Convert an MLScript constrained type to a CTML type. */
def parseTypeConstrained(mlConstraints: List[SubConstraint], mlBody: Term)(using Scope): Type =
  mlConstraints match
    case mlConstraint :: mlConstraints =>
      val body       = parseTypeConstrained(mlConstraints, mlBody)
      val constraint = parseTypeConstraint(mlConstraint)
      TConstrained(body, constraint)
    case Nil =>
      parseType(mlBody)

/** Convert an MLScript subtyping constraint to a CTML type constraint. */
def parseTypeConstraint(mlConstraint: SubConstraint)(using Scope): Constraint =
  val left  = parseType(mlConstraint.lhs)
  val right = parseType(mlConstraint.rhs)
  val dir   = parseTypeDirection(mlConstraint.dir)
  Constraint(left, dir, right)

/** Convert an MLScript subtyping direction to a CTML subtyping direction. */
def parseTypeDirection(mlDir: SubDir): Direction =
  mlDir match
    case SubDir.Sub =>
      Direction.Sub
    case SubDir.Sup =>
      Direction.Super
