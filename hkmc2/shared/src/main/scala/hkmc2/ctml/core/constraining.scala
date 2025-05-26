package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Attach some bounds to a constraining type. */
def attachConstrainingBounds(type_ : Type, bounds: List[Bound])(using ctx: Clauses): Type =
  val filteredBounds = ctx.filterUnsatisfiedBounds(bounds)
  if filteredBounds == Nil then
    type_
  else
    TConstraining(type_, bounds)
