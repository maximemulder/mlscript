package hkmc2.ctml.core.inference

import hkmc2.ctml.core.context.*
import hkmc2.ctml.core.type_.*
import hkmc2.ctml.core.type_.impls.substitute.substitute
import hkmc2.ctml.types.*

/** Instantiate leading universals with explicit type arguments. Guards stay on the result: an
 *  application chooses a type witness but does not assume or discard its obligations. Transparent
 *  aliases expose universals; arbitrary bounds and inferred variables do not select a witness. */
def instantiateExplicit(type_ : Type, arguments: List[Type])(using SubContext): Type =
  instantiateArguments(type_, arguments, Set.empty)

/** Consume arguments without revisiting a transparent alias before a universal is exposed. */
private def instantiateArguments(type_ : Type, arguments: List[Type], seen: Set[TypeVar])
  (using SubContext): Type =
  arguments match
    case Nil => type_
    case argument :: rest => type_ match
      case TUniv(variable, body) =>
        instantiateArguments(body.substitute(variable, argument), rest, Set.empty)
      case TConstrained(body, guard) =>
        TConstrained(instantiateArguments(body, arguments, seen), guard)
      case TVar(variable) if variable.isRigid && !seen.contains(variable) &&
          variable.lowerBound == variable.upperBound =>
        instantiateArguments(variable.upperBound, arguments, seen + variable)
      case _ =>
        throw TypeError(Some(s"Cannot apply a type argument to non-universal type ${type_}."))
