package hkmc2.ctml.core.system

import hkmc2.ctml.types.*

/** Cache used to store, detect, and solve recursive subtyping queries. */
case class SubtypingCache(pairs: Set[(Type, Type)] = Set()):
  /** Check whether two types are in the subtyping cache. */
  def check(sub: Type, sup: Type): Boolean =
    this.pairs.contains((sub, sup))

  /** Add two types to the subtyping cache. */
  def add(sub: Type, sup: Type): SubtypingCache =
    SubtypingCache(this.pairs + ((sub, sup)))
