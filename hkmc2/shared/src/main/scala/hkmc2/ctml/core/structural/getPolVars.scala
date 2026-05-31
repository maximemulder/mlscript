package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getPolVars(using pol: Polarity): Set[(Polarity, TypeVar)] =
    type_ match
      case TVar(var_) =>
        Set((pol, var_))
      case TNeg(body) =>
        body.getPolVars
      case TTuple(left, right) =>
        left.getPolVars ++ right.getPolVars
      case TLam(param, ret) =>
        param.getPolVars(using !pol) ++ ret.getPolVars
      case TUnion(left, right) =>
        left.getPolVars ++ right.getPolVars
      case TInter(left, right) =>
        left.getPolVars ++ right.getPolVars
      case TApp(abs, arg) =>
        abs.getPolVars ++ arg.getPolVars
      case TUniv(var_, body) =>
        body.getPolVars - ((Polarity.Negative, var_)) - ((Polarity.Positive, var_))
      case TConstrained(body, constraint) =>
        body.getPolVars(using !pol) ++ constraint.getPolVars
      case TConstraining(body, constraint) =>
        body.getPolVars ++ constraint.getPolVars
      case TBot | TTop | TClass(_) =>
        Set.empty

extension (constraint: Constraint)
  /** Get the type variables referenced in a subtyping constraint. */
  def getPolVars(using pol: Polarity): Set[(Polarity, TypeVar)] =
    constraint.left.getPolVars(using !pol) ++ constraint.right.getPolVars
