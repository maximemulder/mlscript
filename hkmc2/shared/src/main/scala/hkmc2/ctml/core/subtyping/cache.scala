package hkmc2.ctml.core.subtyping

import hkmc2.ctml.config.*
import hkmc2.ctml.types.*

/** Cache used to store, detect, and solve recursive subtyping queries. */
case class SubtypingCache(pairs: Set[(Type, Type)] = Set()):
  /** Check whether two types are in the subtyping cache. */
  def check(sub: Type, sup: Type): Boolean =
    if config.debug.cacheCheck then
      output(s"CACHE CHECK ${sub} ${sup}")

    val result = this.pairs.contains((sub, sup))

    if config.debug.cacheHit && result then
      output(s"CACHE HIT ${sub} ${sup}")

    if config.debug.cacheMiss && !result then
      output(s"CACHE MISS ${sub} ${sup}")

    result

  /** Add two types to the subtyping cache. */
  def add(sub: Type, sup: Type): SubtypingCache =
    if config.debug.cacheAdd then
      output(s"CACHE ADD ${sub} ${sup}")

    SubtypingCache(this.pairs + ((sub, sup)))
