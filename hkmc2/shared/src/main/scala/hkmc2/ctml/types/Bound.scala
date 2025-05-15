package hkmc2.ctml.types


/** A type bound. */
class Bound(var name: String, var dir: Direction, var type_ : Type)

extension (bound: Bound)
  def c: CtxLevel = CtxBound(bound)

extension (bounds: List[Bound])
  def c: Context = bounds.map(CtxBound(_))
