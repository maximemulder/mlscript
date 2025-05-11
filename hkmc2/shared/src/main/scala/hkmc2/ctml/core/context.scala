package hkmc2.ctml.core

import hkmc2.ctml.types.*

// TODO: Do not use a global mutable counter.
var freshVarCounter = 0

extension (ctx: Context)
  /** Iterate over the term variables of the context. */
  def vars: Iterator[CtxVar] =
    ctx.iterator.flatMap((level) =>
      level match
        case var_ : CtxVar =>
          Some(var_)
        case _ =>
          None
    )

  /** Iterate over the type variables of the context. */
  def typeVars: Iterator[CtxTypeVar] =
    ctx.iterator.flatMap((level) =>
      level match
        case typeVar: CtxTypeVar =>
          Some(typeVar)
        case _ =>
          None
    )

  /** Iterate over the bounds of the context. */
  def bounds: Iterator[Bound] =
    ctx.iterator.flatMap((level) =>
      level match
        case bound: CtxBound =>
          Some(bound.bound)
        case _ =>
          None
    )

  /** Evaluate a function within a context with a new term variable. */
  def withVar[T](varName: String, varType : Type, f: (Context) => T): T =
    var varCtx = CtxVar(varName, varType) :: ctx
    f(varCtx)

  /** Evaluate a function within a context with a new fresh type variable. */
  def withFreshVar[T](f: (String, Context) => T): T =
    var name = getFreshVarName(freshVarCounter)
    freshVarCounter += 1
    var varCtx = CtxTypeVar(name, TypeVarKind.Fresh) :: ctx
    f(name, varCtx)

  /** Get the type of a term variable in the context. */
  def getVarType(name: String): Type =
    var var_ = ctx.vars.find((var_) => var_.name == name)
    if var_ == None then
      throw new Exception(s"Variable '${name}' not found in the context.")

    var_.get.type_

  /** Get a type variable in the context. */
  def getTypeVar(name: String): CtxTypeVar =
    var var_ = ctx.typeVars.find((var_) => var_.name == name)
    if var_ == None then
      throw new Exception(s"Type variable '${name}' not found in the context.")

    var_.get

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(name: String): Boolean =
    ctx.getTypeVar(name).kind == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(name: String): Boolean =
    ctx.getTypeVar(name).kind == TypeVarKind.Rigid
