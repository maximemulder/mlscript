package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.makeJointType
import hkmc2.ctml.types.*

/** Remove Boolean self occurrences from a bound without unfolding structural recursion.
 *  Pointwise, `X ≤ U(X)` iff `X ≤ U(Top)`, and `L(X) ≤ X` iff `L(Bot) ≤ X`, when
 *  substitution traverses only unions, intersections, and complements. All other types are
 *  opaque atoms, even if they contain X: membership in X at the value being tested says nothing
 *  about membership of its fields or function results in X. Guards and quantifiers are opaque
 *  for the same reason. This equivalence needs neither monotonicity nor a recursive witness.
 */
def normalizeRecursiveBound(var_ : TypeVar, dir: Direction, bound: Type): Type =
  val replacement = dir match
    case Direction.Sub => TTop
    case Direction.Super => TBot

  /** Restrict Boolean membership in the bounded variable at the current value only. */
  def restrict(type_ : Type): Type = type_ match
    case TVar(other) if other == var_ => replacement
    case TNeg(body) => restrict(body).negate()
    case TJointType(mode, left, right) => makeJointType(mode, restrict(left), restrict(right))
    case _ => type_

  restrict(bound)
