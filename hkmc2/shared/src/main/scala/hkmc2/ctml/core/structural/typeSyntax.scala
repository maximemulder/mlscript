package hkmc2.ctml.core.structural

import hkmc2.ctml.types.*

/** Compare type syntax modulo renaming of universal binders. Free variables retain their
 *  concrete identity. No bounds or aliases are consulted. */
def sameTypeSyntax(left: Type, right: Type): Boolean =
  if left eq right then true
  else if left.getClass != right.getClass then false
  else if smallPlainType(left) then left == right
  else TypeSyntaxKey(left) == TypeSyntaxKey(right)

/** Plain syntax is already canonical in the absence of binders. Limit this fast path to
 *  shallow trees so ordinary AST equality and hashing cannot unfold a deep shared type. */
private def smallPlainType(type_ : Type, depth: Int = 3): Boolean = type_ match
  case TVar(_) | TClass(_) | TTop | TBot => true
  case _: TUniv => false
  case _ => depth > 0 && type_.components.forall(smallPlainType(_, depth - 1))

/** A hashable key for `sameTypeSyntax`. Retain the original type (including its binder names)
 *  for diagnostics and for transformations that select a representative. */
final class TypeSyntaxKey(val type_ : Type):
  /** Alpha-equivalent types always choose the same representation: the fast-path test depends
   *  only on constructor shape. Both representations have stack-safe equality and hashing. */
  private lazy val syntax: Type | Vector[SyntaxNode] =
    if smallPlainType(type_) then type_ else SyntaxDag.alphaKey(type_)

  /** Compare the canonical representations, computing them only for compatible root shapes. */
  override def equals(other: Any): Boolean = other match
    case key: TypeSyntaxKey =>
      (type_ eq key.type_) || type_.getClass == key.type_.getClass && syntax == key.syntax
    case _ => false

  /** Cache the hash for keys held in persistent collections. */
  override lazy val hashCode: Int = syntax.hashCode
