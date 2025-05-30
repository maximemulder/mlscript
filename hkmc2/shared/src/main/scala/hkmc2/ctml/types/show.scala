package hkmc2.ctml.types


class TypePrinter(val open: Boolean)

extension (type_ : Type)
  /** Convert the type to its string representation. */
  def show(parentOpen: Boolean = false): String =
    val (string, selfOpen) = type_ match
      case _: TBot =>
        ("⊥", false)
      case _: TTop =>
        ("⊤", false)
      case TVar(name) =>
        (name, false)
      case TLam(param, ret) =>
        val components = param :: ret.getLambdaComponents()
        (components.map(_.show(true)).mkString(" → "), true)
      case TUnion(left, right) =>
        val components = left :: right.getUnionComponents()
        (components.map(_.show(true)).mkString(" ∨ "), true)
      case TInter(left, right) =>
        val components = left :: right.getInterComponents()
        (components.map(_.show(true)).mkString(" ∧ "), true)
      case constrained: TConstrained =>
        (s"∀${showVarNames(constrained.vars.reverse)} ◁ {${showBounds(constrained.bounds.reverse)}}. ${constrained.base.show(false)}", true)
      case constraining: TConstraining =>
        (s"${constraining.base.show(false)} ▷ {${showBounds(constraining.bounds)}}", true)

    // If the type is surrounded by spaces in its parent, and has spaces itself, add parentheses
    // around it.
    if parentOpen && selfOpen then
      s"(${string})"
    else
      string

  /** Get the right-recursive components of a lambda type. */
  def getLambdaComponents(): List[Type] =
    type_ match
      case TLam(param, ret) =>
        param :: ret.getInterComponents()
      case _ =>
        type_ :: Nil

  /** Get the right-recursive components of an union type. */
  def getUnionComponents(): List[Type] =
    type_ match
      case TUnion(left, right) =>
        left :: right.getUnionComponents()
      case _ =>
        type_ :: Nil

  /** Get the right-recursive components of an intersection type. */
  def getInterComponents(): List[Type] =
    type_ match
      case TInter(left, right) =>
        left :: right.getInterComponents()
      case _ =>
        type_ :: Nil

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
