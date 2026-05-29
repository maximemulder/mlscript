package hkmc2.ctml.core.structural

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Get the type variables constrained in a type. */
  def getConstrainedVars: Set[TypeVar] =
    type_ match
      case TNeg(body) =>
        body.getConstrainedVars
      case TTuple(left, right) =>
        left.getConstrainedVars ++ right.getConstrainedVars
      case TLam(param, ret) =>
        param.getConstrainedVars ++ ret.getConstrainedVars
      case TUnion(left, right) =>
        left.getConstrainedVars ++ right.getConstrainedVars
      case TInter(left, right) =>
        left.getConstrainedVars ++ right.getConstrainedVars
      case TApp(abs, arg) =>
        abs.getConstrainedVars ++ arg.getConstrainedVars
      case TUniv(var_, body) =>
        body.getConstrainedVars - var_
      case TConstrained(body, constraint) =>
        body.getConstrainedVars ++ constraint.getVars
      case TConstraining(body, constraint) =>
        body.getConstrainedVars ++ constraint.getVars
      case TBot | TTop | TVar(_) | TClass(_) =>
        Set.empty

  /** Check whether a type variable is constrained in a type. */
  def hasConstrainedVar(var_ : TypeVar): Boolean =
    type_.getConstrainedVars.contains(var_)
