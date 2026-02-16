package hkmc2.ctml.parser

import hkmc2.ctml.types.*
import hkmc2.semantics.Elem
import hkmc2.semantics.Term
import hkmc2.semantics.QuantVar

/** Convert an MLScript term to a CTML type. */
def parseType(mlType: Term): Type =
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
def parseTypeTuple(mlLefts: List[Term], mlRight: Term): Type =
  mlLefts match
    case Nil =>
      parseType(mlRight)
    case mlLeft :: mlLefts =>
      val left  = parseType(mlLeft)
      val right = parseType(mlRight)
      TTuple(left, right)

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
          body = TConstrained(body, Constraint(TVar(var_), Direction.Super, bound))
        case None =>
          ()

      mlVar.ub match
        case Some(mlBound) =>
          val bound = parseType(mlBound)
          body = TConstrained(body, Constraint(TVar(var_), Direction.Sub, bound))
        case None =>
          ()

      TUniv(var_, body)
    case Nil =>
      parseType(mlBody)
