package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.abstractions.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*
import hkmc2.ctml.util.*

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify()(using ctx: Context): Type =
    TypeSimplify(type_, TypeSimplifyParams(ctx))

/** Parameters of the type simplification operation. */
class TypeSimplifyParams(val ctx: Context) extends WithContext[TypeSimplifyParams]:
  def getContext = ctx
  def setContext(ctx: Context) = TypeSimplifyParams(ctx)

/** Implementation of the type simplification operation. */
object TypeSimplify extends TypeContextDispatcher[Const[Type], TypeSimplifyParams](TypeSimplifyCombinator[TypeSimplifyParams])
