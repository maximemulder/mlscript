package hkmc2.ctml.core.simplification

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.structural.*
import hkmc2.ctml.core.subtyping.*
import hkmc2.ctml.core.type_.impls.getAllVarPolarities.*
import hkmc2.ctml.types.*

case class NoInlineVars(vars: Set[TypeVar]):
  def contains(var_ : TypeVar): Boolean =
    vars.contains(var_)

object NoInlineVars:
  given empty: NoInlineVars =
    NoInlineVars(Set.empty)

extension (type_ : Type)
  /** Get the polarities at which a variable can be inlined in a type. */
  def getInlinePolarities(var_ : TypeVar)(using ctx: SubContext): Option[Polarities] =
    val polarities = type_.getAllVarPolarities(var_)

    if polarities == Polarities(false, false) then
      return Some(polarities)

    if polarities == Polarities(false, true) then
      if var_.isIndirectRecursive(Polarity.Positive) then
        return None

      return Some(polarities)

    if polarities == Polarities(true, false) then
      if var_.isIndirectRecursive(Polarity.Negative) then
        return None

      return Some(polarities)

    if checkEqual(var_.lowerBound, var_.upperBound) && !var_.isIndirectRecursive(Polarity.Negative) then
      return Some(polarities)

    None
