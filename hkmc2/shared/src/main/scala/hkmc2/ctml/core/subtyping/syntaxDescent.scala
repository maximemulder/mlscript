package hkmc2.ctml.core.subtyping

import hkmc2.ctml.core.structural.SyntaxDag
import hkmc2.ctml.types.*

/** Check strict product descent through literal syntax, without expanding variable bounds.
 *  Each new side must equal an old side's subterm, and at least one must be proper. Concrete
 *  variable and binder names remain exact; this relation supplies no subtyping proof. */
def syntaxDescends(current: (Type, Type), previous: (Type, Type)): Boolean =
  val syntax = SyntaxDag.literal()
  val left = syntax.index(current._1)
  val right = syntax.index(current._2)
  val oldLeft = syntax.index(previous._1)
  val oldRight = syntax.index(previous._2)
  (left != oldLeft || right != oldRight) &&
    syntax.isSubterm(left, oldLeft) && syntax.isSubterm(right, oldRight)
