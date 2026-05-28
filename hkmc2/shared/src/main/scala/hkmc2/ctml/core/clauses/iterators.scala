package hkmc2.ctml.core.clauses

import scala.collection.mutable.Set as MutSet

import hkmc2.ctml.core.*
import hkmc2.ctml.core.var_.*
import hkmc2.ctml.types.*
import hkmc2.ctml.utils.*

// Iteration methods for clauses.

extension (clauses: AsClauses)
  /** Iterate over the clauses. */
  def iterator: Iterator[Clause] =
    clauses.asClauses.iterator

  /** Get the class definitions in the clauses. */
  def classDefs: List[ClassDecl] =
    clauses.iterator.classDefs.toList

  /** Get the term variable declarations in the clauses. */
  def termVarDecls: List[TermVarDecl] =
    clauses.iterator.termVars.toList

  /** Get the type variable declarations in the clauses. */
  def typeVarDecls: List[TypeVarDecl] =
    clauses.iterator.typeVars.toList

  /** Get the type variables declared in the clauses */
  def typeVars: List[TypeVar] =
    clauses.typeVarDecls.map(_.var_)

  /** Check if a type variable declaration appears in the clauses. */
  def hasVar(var_ : TypeVar): Boolean =
    clauses.typeVars.exists(_ == var_)

  /** Get the bounds defined in the clauses. */
  def bounds: List[Bound] =
    clauses.iterator.bounds.toList

  /** Get the bounds of a type variable defined in the clauses. */
  def varBounds(var_ : TypeVar): List[Bound] =
    clauses.iterator.typeVarClauses(var_).typeVarBounds(var_).toList

  /** Get the rightmost bound of a type variable in a type direction defined in the clauses. */
  def varBound(var_ : TypeVar, dir: Direction): Option[Type] =
    clauses.bounds
      .find(bound => bound.var_ == var_ && bound.dir == dir)
      .map(_.type_)

extension (clauses: Clauses)
  /** Map over the clauses. */
  def map(f: Clause => Clause): Clauses =
    Clauses(clauses.elems.map(f))

  /** Map over the bounds in the clauses. */
  def mapBounds(f: Bound => Bound): Clauses =
    clauses
      .map(_ match
        case bound: Bound =>
          f(bound)
        case clause =>
          clause
      )
      .filter(
        _ match
          case bound: Bound =>
            !isBoundImplicit(bound)
          case clause =>
            true
      )

  /** Filter the clauses. */
  def filter(f: Clause => Boolean): Clauses =
    Clauses(clauses.elems.filter(f))

  /** Filter the bounds in the clauses based on a predicate. */
  def filterBounds(f: Bound => Boolean): Clauses =
    clauses.filter(_ match
      case bound: Bound =>
        f(bound)
      case _ =>
        true
    )

    /** Extract the bounds in the clauses based on a predicate. */
  def extractBounds(f: Bound => Boolean): (List[Bound], Clauses) =
    val (bounds, elems) = clauses.elems.partitionMap(_ match
      case bound: Bound if f(bound) =>
        Left(bound)
      case clause =>
        Right(clause)
    )

    (bounds, Clauses(elems))

  /** Remove the declaration and bounds of a type variable in the clauses. */
  def removeTypeVar(var_ : TypeVar): Clauses =
    clauses
      .removeTypeVarBounds(var_)
      .removeTypeVarDecl(var_)

  /** Remove the declaration of a type variable in the clauses. */
  def removeTypeVarDecl(var_ : TypeVar): Clauses =
    Clauses(clauses.elems.filterUntilInclusive(
      _.isTypeVarDecl(var_),
      !_.isTypeVarDecl(var_),
    ).toList)

  /** Remove the bounds of a type variable in the clauses. */
  def removeTypeVarBounds(var_ : TypeVar): Clauses =
    Clauses(clauses.elems.filterUntilInclusive(
      _.isTypeVarDecl(var_),
      !_.isTypeVarBound(var_),
    ).toList)

extension (clauses: Iterator[Clause])
  /** Iterate over the classes defined in the clauses. */
  def classDefs: Iterator[ClassDecl] =
    clauses.flatMap(_ match
      case def_ : ClassDecl =>
        Some(def_)
      case _ =>
        None
    )

  /** Iterate over the term variables defined in the clauses. */
  def termVars: Iterator[TermVarDecl] =
    clauses.flatMap(_ match
      case var_ : TermVarDecl =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the type variables defined in the clauses. */
  def typeVars: Iterator[TypeVarDecl] =
    clauses.flatMap(_ match
      case var_ : TypeVarDecl =>
        Some(var_)
      case _ =>
        None
    )

  /** Iterate over the bounds defined in the clauses. */
  def bounds: Iterator[Bound] =
    clauses.flatMap(_ match
      case bound: Bound =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the bounds of a variable in the clauses. */
  def typeVarBounds(var_ : TypeVar): Iterator[Bound] =
    clauses.flatMap(_ match
      case bound: Bound if bound.var_ == var_ =>
        Some(bound)
      case _ =>
        None
    )

  /** Iterate over the sub-clauses in the scope of a type variable in the clauses. */
  def typeVarClauses(var_ : TypeVar): Iterator[Clause] =
    clauses.takeWhile(_ match
      case TypeVarDecl(declVar, _, _, _) if declVar == var_ =>
        false
      case _ =>
        true
    )

extension (clause: Clause)
  /** Check whether the clause is the declaration of a given type variable.  */
  def isTypeVarDecl(var_ : TypeVar): Boolean =
    clause match
      case TypeVarDecl(declVar, _, _, _) if declVar == var_ =>
        true
      case _ =>
        false

  /** Check whether the clause is a bound on a given type variable. */
  def isTypeVarBound(var_ : TypeVar): Boolean =
    clause match
      case Bound(boundVar, _, _) if boundVar == var_ =>
        true
      case _ =>
        false

extension (bounds: List[Bound])
  def removeDuplicateBounds(): List[Bound] =
    val cache = MutSet[(TypeVar, Direction)]()
    bounds.filter((bound) =>
      cache.contains((bound.var_, bound.dir)) match
        case true =>
          false
        case false =>
          cache.addOne((bound.var_, bound.dir))
          true
    )

  def sortBounds()(using ctx: Context): List[Bound] =
    bounds.sortWith((a, b) => ctx.compareVarLevels(a.var_, b.var_).lt)
