package hkmc2.ctml.core.type_.impls.simplify

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.core.type_.traits.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.traits.TypeDispatcher

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Context): Type =
    Applicator(type_, TypeSimplifyParams(ctx))

/** Parameters of the type simplification operation. */
private class TypeSimplifyParams(val ctx: Context) extends ContextParams[TypeSimplifyParams]:
  def setContext(ctx: Context) = TypeSimplifyParams(ctx)

/** Implementation of the type simplification operation. */
private def Applicator = TypeContextApplicator[Const[Type], TypeSimplifyParams](Dispatcher, Combinator)

private object Dispatcher extends TypeDispatcher[Const[Type], Id, TypeSimplifyParams](Combinator):
  override def apply(constraint: Constraint, p: TypeSimplifyParams): Constraint =
    constraint

private def Combinator = TypeSimplifyCombinator[TypeSimplifyParams]
