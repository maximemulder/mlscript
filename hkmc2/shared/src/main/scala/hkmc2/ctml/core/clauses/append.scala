package hkmc2.ctml.core.clauses

import hkmc2.ctml.core.*
import hkmc2.ctml.core.combine.*
import hkmc2.ctml.types.*

extension (clauses: Clauses)
  // TODO: There are two types of concatenation: Add clauses to a context, and add clauses to other clauses.
  // When adding new clauses to a context, we may want to keep the context simplified.
  // When adding clauses to other clauses, we usually only want to concatenate them ?
  // In both cases it is simpler if the lower levels appear on the left in the code.
  def concat(others: Clauses): Clauses =
    Clauses(others.elems ::: clauses.elems)
