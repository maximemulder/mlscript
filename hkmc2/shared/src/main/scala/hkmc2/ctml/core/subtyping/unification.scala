package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*

def unify(left: Type, right: Type): SubClauses =
  if left == right then
    SubClauses.empty
  else
    SubClauses.empty
