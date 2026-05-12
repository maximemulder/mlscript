package hkmc2.ctml.parser

/** The kind of a declared class or type variable declaration. */
enum DeclKind:
  case Class
  case Type

/** A scope that tracks classes and type variable declarations. */
case class Scope(
  vars_ : Map[String, DeclKind],
  parent: Option[Scope],
):
  /** Get a child scope with a class declaration. */
  def withClass(name: String): Scope =
    Scope(Map((name, DeclKind.Class)), Some(this))

  /** Get a child scope with a type variable declaration. */
  def withType(name: String): Scope =
    Scope(Map((name, DeclKind.Type)), Some(this))

  /** Get a class or type variable declaration for a name, if it exists. */
  def get(name: String): Option[DeclKind] =
    this.vars_.get(name) match
      case Some(kind) =>
        Some(kind)
      case None =>
        this.parent match
          case Some(parent) =>
            parent.get(name)
          case None =>
            None

  override def toString: String =
    this.vars_.keys.mkString(", ") ++ this.parent.toString

object Scope:
  /** The root scope, which is empty. */
  def root = Scope(Map(), None)
