package hkmc2.ctml.core.validation

import hkmc2.ctml.core.clauses.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.types.*

/** Validate the scoping invariants of an open inference result.
  *
  * Keeping this check separate from the transformations makes it possible to strengthen or
  * disable validation without mixing bookkeeping into the inference and simplification logic.
  */
def validateInferenceState(label: String, type_ : Type, outs: SubClauses)(using ctx: SubContext): Unit =
  val declaredVars = ctx.clauses.typeVars.toSet ++ outs.typeVars
  val referencedVars = type_.getVars ++ outs.assertedBounds.iterator.flatMap((bound) =>
    Iterator.single(bound.var_).concat(bound.type_.getVars)
  )
  val missingVars = referencedVars.filterNot(declaredVars.contains).toSet

  assert(
    missingVars.isEmpty,
    s"Invalid inference state after ${label}: variables ${missingVars.mkString(", ")} are not declared.",
  )

  // The transformations of open inference results only transform asserted bounds, so effective
  // bounds must have been summarized beforehand (see `summarizeBounds`).
  assert(
    !outs.hasEffectiveBounds,
    s"Invalid inference state after ${label}: effective bounds have not been summarized.",
  )
