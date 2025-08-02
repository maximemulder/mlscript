package hkmc2.ctml.core.type_

import hkmc2.ctml.core.*
import hkmc2.ctml.core.abstractions.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (type_ : Type)
  /** Simplify the type based on the information available in a context. */
  def simplify2()(using ctx: Context): Type =
    TypeSimplifier.apply(type_, TypeSimplifyParams(ctx))
