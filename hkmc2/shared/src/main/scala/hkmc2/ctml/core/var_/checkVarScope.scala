package hkmc2.ctml.core.var_

import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.context.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.given

extension (type_ : Type)(using ctx: Context)
  /** Find escaped variables within a type. */
  def findEscapedVars(): Set[TypeVar] =
    type_ match
      case TVar(var_) =>
        var_.findEscapedVars()
      case TUniv(var_, body) =>
        given Context = ctx.declTypeVar(var_, TypeVarKind.Rigid)
        body.findEscapedVars()
      case TConstrained(body, constraint) =>
        var escapedVars = body.findEscapedVars()
        escapedVars ++= constraint.findEscapedVars()
        escapedVars
      case TConstraining(body, constraint) =>
        var escapedVars = body.findEscapedVars()
        escapedVars ++= constraint.findEscapedVars()
        escapedVars
      case _ =>
        type_.accumulate(_.findEscapedVars())

extension (bound: Bound)(using ctx: Context)
  /** Find escaped variables within a bound. */
  def findEscapedVars(): Set[TypeVar] =
    bound.var_.findEscapedVars() ++ bound.type_.findEscapedVars()

extension (constraint: Constraint)(using ctx: Context)
  /** Find escaped variables within a constraint. */
  def findEscapedVars(): Set[TypeVar] =
    constraint.left.findEscapedVars() ++ constraint.right.findEscapedVars()

extension (var_ : TypeVar)(using ctx: Context)
  /** Find whether a type variable is escaped. */
  def findEscapedVars(): Set[TypeVar] =
    if !ctx.hasVar(var_) then
      Set(var_)
    else
      Set.empty

extension (ctx: Context)
  /** Find escaped variables within a context. */
  def findEscapedVars(): Set[TypeVar] =
    ctx.clauses match
      case (bound: Bound) :: clauses =>
        given Context = Context(clauses)
        bound.findEscapedVars() ++ Context(clauses).findEscapedVars()
      case (decl: TermVarDecl) :: clauses =>
        given Context = Context(clauses)
        decl.type_.findEscapedVars() ++ Context(clauses).findEscapedVars()
      case _ =>
        Set.empty

extension (clauses: Clauses)(using ctx: Context)
  /** Find escaped variables within some clauses. */
  def findEscapedVars(): Set[TypeVar] =
    clauses.elems match
      case (bound: Bound) :: clauses =>
        given Context = ctx.extend(clauses)
        bound.findEscapedVars() ++ Context(clauses).findEscapedVars()
      case (decl: TermVarDecl) :: clauses =>
        given Context = ctx.extend(clauses)
        decl.type_.findEscapedVars() ++ Context(clauses).findEscapedVars()
      case _ =>
        Set.empty

/** Check whether any variable has escaped a level within this level output. */
def checkEscapedVars(type_ : Type, outs: Clauses, ctx: Context) =
  given Context = ctx
  val vars = type_.findEscapedVars() ++ ctx.findEscapedVars()
  if vars != Set.empty then
    throw new TypeError(Some(s"Escaped variables: ${vars}"))
