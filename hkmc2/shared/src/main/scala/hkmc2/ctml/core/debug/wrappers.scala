package hkmc2.ctml.core.debug

import hkmc2.ctml.types.*

/** Convert a value to a string and print it with the debug print function. */
def output(value : Any*) =
  DebugInfo.output(("  " * DebugInfo.currentCallDepth) + value.map(_.toString()).mkString(" "))

/** Print a debugging message with the context if the context flag is enabled. */
def outputContext(message: String)(using ctx: Context) =
  val fullMessage = if DebugFlags.context then
    message.concat(s" in ${ctx}")
  else
    message

  output(fullMessage)

/** Decorate the subtype constraining function to print debug information. */
def subtypeWithDebug(impl: (Type, Type) => Clauses)(using mode: Mode)(using Context): (Type, Type) => Clauses =
  if mode == Mode.Constrain && !DebugFlags.constrain then
    return impl

  if mode == Mode.Check && !DebugFlags.check then
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
  if !DebugFlags.infer then
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
  if !DebugFlags.join then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"join ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Decorate the type meet function to print debug information. */
def meetWithDebug(impl: (Type, Type) => Type)(using Context): (Type, Type) => Type =
  if !DebugFlags.meet then
    return impl

  (left: Type, right: Type) =>
    outputContext(s"meet ${left} and ${right}")
    val type_ = debugCall(() => impl(left, right))
    output(s"= ${type_}")
    type_

/** Print a type variable declaration as a debug information. */
def debugTypeVar(decl: TypeVarDecl): TypeVarDecl =
  if !DebugFlags.var_ then
    return decl

  output(s"${decl.var_} ${decl.kind}")
  decl

/** Decorate the type variable quantification function to print debug information. */
def debugQuantifyVar(impl: (Type, TypeVar, Type, Type) => Type)(using Context): (Type, TypeVar, Type, Type) => Type =
  if !DebugFlags.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, lowerBound: Type, upperBound: Type) =>
    outputContext(s"quantify ${var_} with ${lowerBound} and ${upperBound} in ${type_}")
    val newType = impl(type_, var_, lowerBound, upperBound)
    output(s"= ${newType}")
    newType

/** Decorate the type variable inlining function to print debug information. */
def debugInlineVar(impl: (Type, TypeVar, Type) => Type)(using Context): (Type, TypeVar, Type) => Type =
  if !DebugFlags.var_ then
    return impl

  (type_ : Type, var_ : TypeVar, bound: Type) =>
    outputContext(s"inline ${var_} with ${bound} in ${type_}")
    val newType = impl(type_, var_, bound)
    output(s"= ${newType}")
    newType

/** Decorate the type variable ignoring function to print debug information. */
def debugIgnoreVar(impl: (Type, TypeVar) => Type)(using Context): (Type, TypeVar) => Type =
  if !DebugFlags.var_ then
    return impl

  (type_ : Type, var_ : TypeVar) =>
    outputContext(s"ignore ${var_} in ${type_}")
    val newType = impl(type_, var_)
    output(s"= ${newType}")
    newType

/** Register and call a function in the debug environment. */
def debugCall[T](f: () => T): T =
  if DebugInfo.currentStepCount >= DebugInfo.maxStepCount then
    throw Exception("Exceeded maximum step count.")

  if DebugInfo.currentCallDepth >= DebugInfo.maxCallDepth then
    throw Exception("Exceeded maximum call depth.")

  DebugInfo.currentStepCount += 1
  DebugInfo.currentCallDepth += 1

  try
    f()
  finally
    DebugInfo.currentCallDepth -= 1
