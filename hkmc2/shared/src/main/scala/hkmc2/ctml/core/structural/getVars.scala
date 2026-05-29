package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the type variables referenced in a type. */
  def getVars: Set[TypeVar] =
    type_ match
      case TVar(var_) =>
        Set(var_)
      case TNeg(body) =>
        body.getVars
      case TTuple(left, right) =>
        left.getVars ++ right.getVars
      case TLam(param, ret) =>
        param.getVars ++ ret.getVars
      case TUnion(left, right) =>
        left.getVars ++ right.getVars
      case TInter(left, right) =>
        left.getVars ++ right.getVars
      case TApp(abs, arg) =>
        abs.getVars ++ arg.getVars
      case TUniv(var_, body) =>
        body.getVars - var_
      case TConstrained(body, constraint) =>
        body.getVars ++ constraint.getVars
      case TConstraining(body, constraint) =>
        body.getVars ++ constraint.getVars
      case TBot | TTop | TClass(_) =>
        Set.empty

  /** Check whether a type variable is referenced in a type. */
  def hasVar(var_ : TypeVar): Boolean =
    type_.getVars.contains(var_)

extension (constraint: Constraint)
  /** Get the type variables referenced in a subtyping constraint. */
  def getVars: Set[TypeVar] =
    constraint.left.getVars ++ constraint.right.getVars

  /** Check whether a type variable is referenced in a subtyping constraint. */
  def hasVar(var_ : TypeVar): Boolean =
    constraint.getVars.contains(var_)
