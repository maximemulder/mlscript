package hkmc2.ctml.core.subtyping

import hkmc2.ctml.types.*

def unify(left: Type, right: Type): Clauses =
  if left == right then
    Clauses.empty
  else
    Clauses.empty
