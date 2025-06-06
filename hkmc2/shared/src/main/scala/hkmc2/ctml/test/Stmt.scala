package hkmc2.ctml.test

import hkmc2.ctml.types.*

/** A CTML statement, which is used in the type system. */
sealed trait Stmt

/** A class declaration. */
case class StmtClassDecl(name: String) extends Stmt

/** A type variable declaration. */
case class StmtTypeDecl(name: String) extends Stmt

/** A type variable assignment. */
case class StmtTypeVar(name: String, type_ : Type) extends Stmt

/** An expression variable declaration. */
case class StmtExprDecl(name: String, type_ : Type) extends Stmt

/** An expression variable assignment. */
case class StmtExprVar(name: String, expr: Expr) extends Stmt

/** An expression. */
case class StmtExpr(expr: Expr) extends Stmt

/** A relation between two types. */
case class StmtTypeRel(rel: TypeRel, left: Type, right: Type) extends Stmt

/** A relation between two types. */
enum TypeRel:
  /** The two types are equal. */
  case Eq
  /** The two types are not equal. */
  case Ne
  /** The left type is a subtype of the right type. */
  case Sub
  /** The left type is a supertype of the right type. */
  case Sup
