package hkmc2.ctml.core.inference

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*

/** Infer the types and obligations of a dependency component.
 *  Recursive occurrences share uninstantiated provisional variables while inferring a group
 *  transformer. One polymorphic fixed-point signature then identifies its inputs and outputs.
 *  The signature uses the known function shapes, preventing whole-function bounds from growing
 *  into nested polymorphic cycles. Its application stays at the caller's level: closing another
 *  level just around the fixed point separates its instance from the transformer's assumptions
 *  and makes guarded recursive constraints expensive to solve. */
def inferBindingGroup(group: BindingGroup)(using ctx: TypeContext): (List[Type], SubClauses) =
  if !group.recursive then
    val (type_, clauses) = infer(group.bindings.head.expr)
    (List(type_), clauses)
  else
    val functional = ctx.withInferLevel: inner =>
      val decls = group.bindings.map(_ => inner.sub.declInferVar())
      val initial = SubClauses(decls)
      val recursiveCtx = group.bindings.zip(decls).foldLeft(inner.extendSub(initial)):
        case (ctx, (binding, decl)) =>
          ctx.extendTerm(TermVarDecl(binding.variable, TVar(decl.var_)))
      val (bodyTypes, clauses) = group.bindings.foldLeft((List.empty[Type], initial)):
        case ((types, clauses), binding) =>
          val (type_, outs) = inferSeq(binding.expr, clauses)(using recursiveCtx)
          (types :+ type_, outs)
      val type_ = decls.foldRight(packTypes(bodyTypes))((decl, ret) => TLam(TVar(decl.var_), ret))
      (type_, clauses)
    val shapes = group.bindings.map(binding => recursiveShape(binding.expr))
    val variables = shapes.flatMap(_._2)
    val results = shapes.map(_._1)
    val result = packTypes(results)
    val transformer = results.foldRight(result)(TLam(_, _))
    val fixType = variables.foldRight[Type](TLam(transformer, result))(TUniv(_, _))
    val (fixed, fixedClauses) = applyType(fixType, functional, SubClauses.empty)
    if group.bindings.size == 1 then (List(fixed), fixedClauses)
    else
      val decls = group.bindings.map(_ => ctx.sub.declInferVar())
      val types = decls.map(decl => TVar(decl.var_))
      val clauses = typingSubtypeSeq(fixed, packTypes(types), fixedClauses.concat(SubClauses(decls)))
      (types, clauses)

/** Close a component's inference level. Top-level bindings have no enclosing term parameters;
 *  local recursive components consist of closures, so their bodies can be generalized here. */
def inferGeneralizedBindings(group: BindingGroup)(using ctx: TypeContext): List[Type] =
  ctx.sub.withLevel: sub =>
    val (types, clauses) = inferBindingGroup(group)(using ctx.copy(sub = sub))
    types.map(sub.processLevel(sub.level, _, clauses))

/** Construct a function shape and the variables to quantify in the fixed-point signature.
 *  Parameter annotations and Unit domains are fixed; other domains and the result are fresh. */
private def recursiveShape(expr: Expr): (Type, List[TypeVar]) = expr match
  case ELam(param, body) =>
    val (domain, domainVars) = param match
      case None => (TClass(ClassVar("Unit")), Nil)
      case Some(LambdaParam(_, Some(annotation))) => (annotation, Nil)
      case Some(LambdaParam(_, None)) => freshRecursiveType()
    val (result, resultVars) = recursiveShape(body)
    (TLam(domain, result), resultVars ::: domainVars)
  case EAscr(body, _) if body.isFunction => recursiveShape(body)
  case _ => freshRecursiveType()

/** Allocate a parameter or result variable to quantify in the fixed-point signature. */
private def freshRecursiveType(): (Type, List[TypeVar]) =
  val variable = newFreshVar()
  (TVar(variable), List(variable))

/** Package the results of a nonempty component in an internal binary product. */
private def packTypes(types: List[Type]): Type = types match
  case head :: Nil => head
  case head :: tail => TTuple(head, packTypes(tail))
  case Nil => throw new IllegalArgumentException("A binding component must be nonempty.")

/** Apply an inferred function type, including the constraints of its function and argument. */
private[inference] def applyType(lamType: Type, argType: Type, clauses: SubClauses)
  (using ctx: TypeContext): (Type, SubClauses) =
  val retDecl = ctx.sub.declInferVar()
  val retType = TVar(retDecl.var_)
  val outs = typingSubtypeSeq(lamType, TLam(argType, retType), clauses.concat(retDecl.asSubClauses))
  (retType, outs)

/** Type a binding's continuation as an abstraction, then apply it to the inferred definitions.
 *  This is the core lambda-calculus let rule: it preserves first-class polymorphism and checks
 *  initializer constraints even when the continuation does not use the bound variable. Recursive
 *  closures are generalized together before being passed to the continuation. */
private[inference] def inferLet(group: BindingGroup, body: Expr)
  (using ctx: TypeContext): (Type, SubClauses) =
  val continuation = group.bindings.foldRight(body): (binding, body) =>
    ELam(Some(LambdaParam(binding.variable, None)), body)
  val (continuationType, continuationClauses) = infer(continuation)
  val (types, clauses) = if group.recursive then
    (inferGeneralizedBindings(group)(using ctx.extendSub(continuationClauses)), continuationClauses)
  else ctx.seq(inferBindingGroup(group), continuationClauses)
  types.foldLeft((continuationType, clauses)):
    case ((lamType, clauses), argType) => applyType(lamType, argType, clauses)
