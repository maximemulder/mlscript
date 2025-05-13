package hkmc2.ctml.types


/** A type bound. */
class Bound(var name: String, var dir: Direction, var type_ : Type)

type Bounds = List[Bound]

extension (bounds: Bounds)
  def c: Context =
    bounds.map(CtxBound(_))
