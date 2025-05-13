package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Substitute a variable by a type in another type. */
def substitute(type_ : Type, varName: String, other: Type)(using ctx: Context): Type =
  type_ match
    case TBot =>
      TBot
    case TTop =>
      TTop
    case var_ : TVar if var_.name == varName =>
      other
    case _: TVar =>
      type_
    case lam: TLam =>
      val param = substitute(lam.param, varName, other)
      val ret   = substitute(lam.ret,   varName, other)
      TLam(param, ret)
    case union: TUnion =>
      val left  = substitute(union.left,  varName, other)
      val right = substitute(union.right, varName, other)
      join(left, right)
    case inter: TInter =>
      val left  = substitute(inter.left,  varName, other)
      val right = substitute(inter.right, varName, other)
      join(left, right)
    case constraining: TConstraining =>
      val base = substitute(constraining.base, varName, other)
      val bounds = substituteBounds(constraining.bounds, varName, other)
      TConstraining(base, bounds)

/** Substitute a variable by a type in a list bounds. */
def substituteBounds(bounds: List[Bound], varName: String, other: Type)(using ctx: Context): List[Bound] =
  bounds.iterator
    .filter((bound) => bound.name != varName)
    .map((bound) => Bound(bound.name, bound.dir, substitute(bound.type_, varName, other)))
    .toList
