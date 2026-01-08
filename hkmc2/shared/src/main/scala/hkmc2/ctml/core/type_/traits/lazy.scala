package hkmc2.ctml.core.type_.traits

import hkmc2.ctml.types.*
import hkmc2.ctml.core.config.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.syntax.Keyword.`override`
import hkmc2.ctml.util.Const

/** Applicator that applies another applicator on the components of a type, and then applies a
 *  combinator only of the components of that typed have changed. */
abstract class TypeLazyDispatcher[B[+_], P](last: TypeCombinator[Const[Type], B, P]) extends TypeApplicator[Const[Type], P], ConstraintApplicator[B, P]:
  override def apply(type_ : Type, p: P)(using first: TypeApplicator[Const[Type], P]): Type =
    type_ match
      case TBot =>
        last.bot(p)
      case TTop =>
        last.top(p)
      case TVar(var_) =>
        last.var_(var_)
      case TTuple(left, right) =>
        val newLeft  = first.apply(left, p);
        val newRight = first.apply(right, p);
        if newLeft == left && newRight == right then
          return type_
        last.tuple(newLeft, newRight, p)
      case TLam(param, ret) =>
        val newParam = first.apply(param, p);
        val newRet = first.apply(ret, p);
        if newParam == param && newRet == ret then
          return type_
        last.lam(newParam, newRet, p)
      case TUnion(left, right) =>
        val newLeft = first.apply(left, p);
        val newRight = first.apply(right, p);
        if newLeft == left && newRight == right then
          return type_
        last.union(newLeft, newRight, p)
      case TInter(left, right) =>
        val newLeft = first.apply(left, p);
        val newRight = first.apply(right, p);
        if newLeft == left && newRight == right then
          return type_
        last.inter(newLeft, newRight, p)
      case TApp(abs, arg) =>
        val newAbs = first.apply(abs, p);
        val newArg = first.apply(arg, p);
        if newAbs == abs && newArg == arg then
          return type_
        last.app(newAbs, newArg, p)
      case TUniv(var_, body) =>
        val newBody = first.apply(body, p);
        if newBody == body then
          return type_
        last.univ(var_, newBody, p)
      case TConstrained(body, constraint) =>
        val newBody = first.apply(body, p);
        val newConstraint = this.apply(constraint, p);
        if newBody == body && newConstraint == constraint then
          return type_
        last.constrained(newBody, newConstraint, p)
      case TConstraining(body, constraint) =>
        val newBody = first.apply(body, p);
        val newConstraint = this.apply(constraint, p);
        if newBody == body && newConstraint == constraint then
          return type_
        last.constraining(newBody, newConstraint, p)
