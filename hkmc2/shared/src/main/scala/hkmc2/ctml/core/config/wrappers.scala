package hkmc2.ctml.core.config

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*

/** Convert a value to a string and print it with the debug print function. */
def output(value : Any*): Unit =
  if Debug.depth.exists(_ < Config.currentCallDepth) then
    return

  if Debug.depth.exists(_ == Config.currentCallDepth) then
    Config.output(("  " * Config.currentCallDepth) + "...")

  Config.output(("  " * Config.currentCallDepth) + value.map(_.toString()).mkString(" "))

/** Print a debugging message with the context if the context flag is enabled. */
def outputContext(message: String)(using ctx: Context) =
  val fullMessage = if Debug.context then
    message.concat(s" in ${cleanContext(ctx)}")
  else
    message

  output(fullMessage)

/** Clean the context by removing definitions from the prelude. */
def cleanContext(ctx: Context): Context =
  ctx.map(_.takeWhile(_ match
    case TermVarDecl("||", _) =>
      false
    case _ =>
      true
  ))

/** Decorate the subtype constraining function to print debug information. */
def subtypeWithDebug(impl: (Type, Type) => Clauses)(using mode: Mode)(using Context): (Type, Type) => Clauses =
  if mode == Mode.Constrain && !Debug.constrain then
    return impl

  if mode == Mode.Check && !Debug.check then
    return impl

  (sub: Type, sup: Type) =>
    try
      outputContext(s"${mode} ${sub} ≤ ${sup}")
      val outs = debugCall(() => impl(sub, sup))
      output(s"OK ⇝ ${outs}")
      outs
    catch
      case error: TypeError =>
        output("FAIL")
        throw error

/** Decorate the type inference function to print debug information. */
def inferWithDebug(impl: Expr => (Type, Clauses))(using Context): Expr => (Type, Clauses) =
  if !Debug.infer then
    return impl

  (expr: Expr) =>
    outputContext(s"infer ${expr}")

    try
      val (type_, outs) = debugCall(() => impl(expr))
      output(s"OK ${type_} ⇝ ${outs}")
      (type_, outs)
    catch
      case error: TypeError =>
        output("FAIL")
        throw error

/** Decorate the type join function to print debug information. */
def joinWithDebug(impl: (Type, Type) => Type)(using Context): (Type, Type) => Type =
  if !Debug.join then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def meetWithDebug(impl: (Type, Type) => Type)(using Context): (Type, Type) => Type =
  if !Debug.meet then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Print a type variable declaration as a debug information. */
def debugTypeVar(decl: TypeVarDecl): TypeVarDecl =
  if !Debug.var_ then
    return decl

  output(s"${decl.var_} ${decl.kind}${decl.original match
    case Some(original) =>
      s" freshen ${original}"
    case None =>
      ""
  }")

  decl

/** Decorate the type variable quantification function to print debug information. */
def debugQuantifyVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses) =
  if !Debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"quantify ${var_} in ${type_}")
    val (newType, newOuts) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts)

/** Decorate the type variable inlining function to print debug information. */
def debugInlineVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses) =
  if !Debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"inline ${var_} in ${type_}")
    val (newType, newOuts) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts)

/** Decorate the type variable ignoring function to print debug information. */
def debugIgnoreVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses) =
  if !Debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"ignore ${var_} in ${type_}")
    val (newType, newOuts) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts)

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if Config.currentStepCount >= Config.maxStepCount then
    throw Exception("Exceeded maximum step count.")

  if Config.currentCallDepth >= Config.maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  Config.currentStepCount += 1
  Config.currentCallDepth += 1

  try
    f()
  finally
    Config.currentCallDepth -= 1
