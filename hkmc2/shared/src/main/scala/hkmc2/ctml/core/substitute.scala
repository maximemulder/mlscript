package hkmc2.ctml.core

import hkmc2.ctml.types.*

/** Replace a type variable by a subtitute type in a type. */
def substitute(type_ : Type, varName: String, subsitute: Type)(using ctx: Context): Type =
  type_ match
    case TBot =>
      TBot
    case TTop =>
      TTop
    case var_ : TVar if var_.name == varName =>
      subsitute
    case _: TVar =>
      type_
    case lam: TLam =>
      val param = substitute(lam.param, varName, subsitute)
      val ret   = substitute(lam.ret,   varName, subsitute)
      TLam(param, ret)
    case union: TUnion =>
      val left  = substitute(union.left,  varName, subsitute)
      val right = substitute(union.right, varName, subsitute)
      join(left, right)
    case inter: TInter =>
      val left  = substitute(inter.left,  varName, subsitute)
      val right = substitute(inter.right, varName, subsitute)
      join(left, right)
    case constraining: TConstraining =>
      val base = substitute(constraining.base, varName, subsitute)
      val bounds = substituteBounds(constraining.bounds, varName, subsitute)
      TConstraining(base, bounds)

/** Substitute a variable by a type in a list bounds. */
def substituteBounds(bounds: List[Bound], varName: String, subsitute: Type)(using ctx: Context): List[Bound] =
  bounds.iterator
    .filter((bound) => bound.name != varName)
    .map((bound) => Bound(bound.name, bound.dir, substitute(bound.type_, varName, subsitute)))
    .toList
