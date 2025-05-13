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

  /** Get the type of a term variable in the context. */
  def getVarType(varName: String): Type =
    var var_ = ctx.vars.find((var_) => var_.name == varName)
    if var_ == None then
      throw new TypeError(s"Variable '${varName}' not found in the context.")

    var_.get.type_

  /** Get a type variable in the context. */
  def getTypeVar(varName: String): CtxTypeVar =
    var var_ = ctx.typeVars.find((var_) => var_.name == varName)
    if var_ == None then
      throw new TypeError(s"Type variable '${varName}' not found in the context.")

    var_.get

  /** Check whether a type variable is a fresh variable in the context. */
  def isTypeVarFresh(varName: String): Boolean =
    ctx.getTypeVar(varName).kind == TypeVarKind.Fresh

  /** Check whether a type variable is a rigid variable in the context. */
  def isTypeVarRigid(varName: String): Boolean =
    ctx.getTypeVar(varName).kind == TypeVarKind.Rigid

  /** Get the lower bound of a type variable in the context. */
  def getVarLowerBound(varName: String): Type =
    ctx.bounds
      .filter((bound) => bound.name == varName && bound.dir == Direction.Super)
      .foldRight(TBot: Type)((bound, type_) =>
        given Context = ctx
        join(type_, bound.type_)
      )

  /** Get the upper bound of a type variable in the context. */
  def getVarUpperBound(varName: String): Type =
    ctx.bounds
      .filter((bound) => bound.name == varName && bound.dir == Direction.Sub)
      .foldRight(TTop: Type)((bound, type_) =>
        given Context = ctx
        meet(type_, bound.type_)
      )

  /** Evaluate a function within a context with a new fresh type variable. */
  def withFreshVarLevel(f: (String, Context) => (Type, List[Bound])): (Type, List[Bound]) =
    withFreshVar((varName, varCtx) =>
      val (type_ , bounds) = f(varName, varCtx)
      val newCtx = concatCtxs(ctx, bounds.c)
      val polarities =
        given Polarity = Polarity.Positive
        getVarPolarities(type_, varName)
      polarities match
        case Polarities(true, true) =>
          // TODO: Polymorphism.
          (type_, bounds)
        case Polarities(true, false) =>
          val upperBound = newCtx.getVarUpperBound(varName)
          given Context = newCtx
          val newType = substitute(type_, varName, upperBound)
          (newType, bounds)
        case Polarities(false, true) =>
          val lowerBound = newCtx.getVarLowerBound(varName)
          given Context = newCtx
          val newType = substitute(type_, varName, lowerBound)
          (newType, bounds)
        case Polarities(false, false) =>
          (type_, bounds)
    )

/** Concatenante some contexts. */
def concatCtxs(ctxs: Context*): Context =
  ctxs.flatten.toList

/** Evaluate a function within a context with a new term variable. */
def withVar[T](varName: String, varType : Type, f: (Context) => T): T =
  var varCtx = CtxVar(varName, varType) :: Nil
  f(varCtx)

/** Evaluate a function within a context with a new fresh type variable. */
def withFreshVar[T](f: (String, Context) => T): T =
  var varName = getFreshVarName(freshVarCounter)
  freshVarCounter += 1
  var varCtx = CtxTypeVar(varName, TypeVarKind.Fresh) :: Nil
  f(varName, varCtx)
