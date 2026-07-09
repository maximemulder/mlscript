package hkmc2.ctml.core.context

import scala.math.Ordering.ordered

import hkmc2.ctml.config.*
import hkmc2.ctml.core.*
import hkmc2.ctml.core.clauses.typeVarDecls
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.isConstraining
import hkmc2.ctml.types.*

extension (ctx: SubContext)
  /** Extend the context with one or several clauses. */
  def extend(clauses: AsSubClauses*): SubContext =
    if clauses.flatMap(_.asSubClauses).length == 0 then
      return ctx

    clauses
      .reverse
      .flatMap(_.asSubClauses)
      .foldRight(ctx)((clause, ctx) => ctx.extendOne(clause))

  /** Append a clause at the end of the clauses. */
  def extendOne(clause: SubClause): SubContext =
    SubContext(clause :: ctx.clauses, ctx.cache, ctx.level)

extension (ctx: TypeContext)
  /** Map over the subtyping context of this typing context. */
  def mapSub(f: SubContext => SubContext): TypeContext =
    ctx.copy(sub = f(ctx.sub))

  /** Extend the subtyping context with one or several clauses. */
  def extendSub(clauses: AsSubClauses*): TypeContext =
    ctx.mapSub(_.extend(clauses*))

  /** Extend the term context with a term variable declaration. */
  def extendTerm(decl: TermVarDecl): TypeContext =
    ctx.copy(terms = decl :: ctx.terms)
