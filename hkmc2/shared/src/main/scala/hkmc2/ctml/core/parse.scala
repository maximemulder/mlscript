package hkmc2.ctml.core

import hkmc2.semantics.Term
import hkmc2.ctml.types.*

extension (term: Term)
  /** Convert a MLScript term to a CTML type, or throw an exception if that term is not a valid
   * CTML type. */
  def toType(): Type =
    term match
      case Term.Tup(elems) =>
        if elems.length != 1 then
          throw ParseError(term)

        val elem = elems(0)
        if elem.subTerms.length != 1 then
          throw ParseError(term)

        elem.subTerms(0).toType()
      case Term.Ref(symbol) =>
        TVar(symbol.nme)
      case Term.FunTy(param, ret, _) =>
        TFun(param.toType(), ret.toType())
      case _ =>
        throw ParseError(term)
