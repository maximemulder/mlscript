package hkmc2.ctml.config

import hkmc2.ctml.types.*
import hkmc2.ctml.core.context.*

/** Convert a value to a string and print it with the debug print function. */
def output(value : Any*): Unit =
  if config.debug.depth.exists(_ < config.currentCallDepth) then
    return

  if config.debug.depth.exists(_ == config.currentCallDepth) then
    config.output(("  " * config.currentCallDepth) + "...")

  config.output(("  " * config.currentCallDepth) + value.map(_.toString()).mkString(" "))

def debug(value: Any*): Unit =
  if !config.debug.enabled then
    return

  output(value*)

def debugContext(value: Any*)(using ctx: Context): Unit =
  if !config.debug.enabled then
    return

  outputContext(value.map(_.toString()).mkString(" "))

/** Print a debugging message with the context if the context flag is enabled. */
def outputContext(message: String)(using ctx: Context) =
  val fullMessage = if config.debug.context then
    message.concat(s" in ${cleanContext(ctx)}")
  else
    message

  output(fullMessage)

/** Clean the context by removing definitions from the prelude. */
def cleanContext(ctx: Context): Context =
  ctx.map(_.takeWhile(_ match
    case TypeVarDecl(TypeVar("Sup"), _, _, _) =>
      false
    case _ =>
      true
  ))

var mode: RefineMode = RefineMode.Constrain

/** Run a function in type checking mode, where constrainings are not displayed. */
def withCheckingMode[T](f: => T): T =
  val oldMode = mode
  try
    mode = RefineMode.Check
    f
  finally
    mode = oldMode

/** Decorate the subtype constraining function to print debug information. */
def subtypeWithDebug(impl: (Type, Type) => Clauses)(using Context): (Type, Type) => Clauses =
  if mode == RefineMode.Constrain && !config.debug.constrain then
    return impl

  if mode == RefineMode.Check && !config.debug.check then
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
  if !config.debug.infer then
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

/** Decorate the type extrusion function to print debug information. */
def extrudeWithDebug(impl: Type => (Type, Clauses))(using Context): Type => (Type, Clauses) =
  if !config.debug.extrude then
    return impl

  (type_ : Type) =>
    outputContext(s"extrude ${type_}")

    try
      val (res, outs) = debugCall(() => impl(type_))
      output(s"OK ${res} ⇝ ${outs}")
      (res, outs)
    catch
      case error: TypeError =>
        output("FAIL")
        throw error

/** Decorate the type join function to print debug information. */
def joinWithDebug(impl: (Type, Type) => Type)(using Context): (Type, Type) => Type =
  if !config.debug.join then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def meetWithDebug(impl: (Type, Type) => Type)(using Context): (Type, Type) => Type =
  if !config.debug.meet then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Print a type variable declaration as a debug information. */
def debugTypeVar(decl: TypeVarDecl): Unit =
  if !config.debug.var_ then
    return

  output(s"${decl.var_} ${decl.kind} level ${decl.level}")

/** Print an inference type variable declaration as a debug information. */
def debugInferVar(decl: TypeVarDecl): Unit =
  if !config.debug.var_ then
    return

  output(s"${decl.var_} ${decl.kind} infer level ${decl.level}")

/** Print a freshened type variable declaration as a debug information. */
def debugFreshVar(decl: TypeVarDecl): Unit =
  if !config.debug.var_ then
    return

  output(s"${decl.var_} ${decl.kind} freshen ${decl.original.get} level ${decl.level}")

/** Print an extruded type variable declaration as a debug information. */
def debugExtrudeVar(decl: TypeVarDecl): Unit =
  if !config.debug.var_ then
    return

  output(s"${decl.var_} ${decl.kind} extrude ${decl.original.get} level ${decl.level}")

/** Decorate the type variable quantification function to print debug information. */
def debugQuantifyVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses, Boolean))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses, Boolean) =
  if !config.debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"quantify ${var_} in ${type_}")
    val (newType, newOuts, b) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts, b)

/** Decorate the type variable inlining function to print debug information. */
def debugInlineVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses, Boolean))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses, Boolean) =
  if !config.debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"inline ${var_} in ${type_}")
    val (newType, newOuts, b) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts, b)

/** Decorate the type variable ignoring function to print debug information. */
def debugIgnoreVar(impl: (Type, TypeVar, Clauses) => (Type, Clauses, Boolean))(using Context): (Type, TypeVar, Clauses) => (Type, Clauses, Boolean) =
  if !config.debug.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, outs: Clauses) =>
    outputContext(s"ignore ${var_} in ${type_}")
    val (newType, newOuts, b) = impl(type_, var_, outs)
    output(s"= ${newType}")
    (newType, newOuts, b)

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if config.currentStepCount >= config.maxStepCount then
    throw Exception("Exceeded maximum step count.")

  if config.currentCallDepth >= config.maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  config.currentStepCount += 1
  config.currentCallDepth += 1

  val builder = StringBuilder()
  val prevOutput = config.output
  config.output = (message) => builder.append(s"${message}\n")

  try
    val res = f()
    val builderString = builder.toString
    if builderString != "" then
      prevOutput(builderString.stripLineEnd)
    res
  catch
    case e: Exception =>
      val builderString = builder.toString
      if builderString != "" then
        prevOutput(builderString.stripLineEnd)
      throw e
  finally
    config.currentCallDepth -= 1
    config.output = prevOutput
