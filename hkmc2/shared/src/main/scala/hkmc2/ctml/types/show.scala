package hkmc2.ctml.types

extension (type_ : Type)
  /** Convert the type to its string representation. */
  def show(): String =
    type_ match
      case _: TBot =>
        "⊥"
      case _: TTop =>
        "⊤"
      case var_ : TVar =>
        var_.name
      case lam: TLam =>
        s"${lam.param.show()} → ${lam.ret.show()}"
      case union: TUnion =>
        s"${union.left.show()} ∨ ${union.right.show()}"
      case inter: TInter =>
        s"${inter.left.show()} ∧ ${inter.right.show()}"
      case constrained: TConstrained =>
        s"∀${showVarNames(constrained.vars)} ⇐ {${showBounds(constrained.bounds)}}. ${constrained.base.show()}"
      case constraining: TConstraining =>
        s"${constraining.base.show()} ⇒ {${showBounds(constraining.bounds)}}"

extension (bound: Bound)
  /** Convert the bound to its string representation. */
  def show(): String =
    return s"${bound.name} ${bound.dir.show()} ${bound.type_.show()}"

extension (dir: Direction)
  /** Convert the subtyping direction to its string representation. */
  def show(): String =
    dir match
      case Direction.Sub   => "≤"
      case Direction.Super => "≥"

extension (pol: Polarity)
  /** Convert the type polarity to its string representation. */
  def show(): String =
    pol match
      case Polarity.Negative => "−"
      case Polarity.Positive => "+"

extension (mode: Mode)
  /** Convert the typing mode to its string representation. */
  def show(): String =
    mode match
      case Mode.Constrain => "constrain"
      case Mode.Check     => "check"

extension (clauses: Clauses)
  /** Convert the clauses to their string representation. */
  def show(): String =
    clauses.elems.map(_.show()).mkString(", ")

extension (clause: Clause)
  /** Convert the clause to its string representation. */
  def show(): String =
    clause match
      case var_ : TermVar =>
        var_.show()
      case var_ : TypeVar =>
        var_.show()
      case bound: Bound =>
        bound.show()

extension (var_ : TermVar)
  /** Convert the term variable to its string representation. */
  def show(): String =
    s"${var_.name}: ${var_.type_.show()}"

extension (var_ : TypeVar)
  /** Convert the type variable to its string representation. */
  def show(): String =
    s"${var_.name} ${var_.kind.show()}"

extension (kind: TypeVarKind)
  /** Convert thea type variable kind to its string representation. */
  def show(): String =
    kind match
      case TypeVarKind.Rigid => "rigid"
      case TypeVarKind.Fresh => "fresh"

/** Convert a list of bounds to its string representation. */
def showBounds(bounds: List[Bound]): String =
  bounds.map(_.show()).mkString(", ")

/** Convert a list of variable names to its string representation. */
def showVarNames(vars: List[String]): String =
  vars.mkString(", ")
