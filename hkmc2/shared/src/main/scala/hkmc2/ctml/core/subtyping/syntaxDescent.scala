package hkmc2.ctml.core.subtyping

import java.util.IdentityHashMap
import scala.collection.mutable

import hkmc2.ctml.types.*

/** Check strict product descent through literal syntax, without expanding variable bounds.
 *  Each new side must equal an old side's subterm, and at least one must be proper. Concrete
 *  variable and binder names remain exact; this relation supplies no subtyping proof. */
def syntaxDescends(current: (Type, Type), previous: (Type, Type)): Boolean =
  val syntax = SyntaxDag()
  val left = syntax.index(current._1)
  val right = syntax.index(current._2)
  val oldLeft = syntax.index(previous._1)
  val oldRight = syntax.index(previous._2)
  (left != oldLeft || right != oldRight) &&
    syntax.isSubterm(left, oldLeft) && syntax.isSubterm(right, oldRight)

/** Constructor data independent of nested types, suitable for ordinary hashing. */
private enum SyntaxHead:
  case Bottom, Top, Negation, Tuple, Arrow, Application
  case Variable(name: String)
  case Class(name: String)
  case Field(name: String)
  case Joint(mode: JointMode)
  case Universal(name: String)
  case Guard(direction: Direction)

/** An exact syntax signature whose children are local canonical integers. */
private case class SyntaxNode(head: SyntaxHead, children: List[Int])

/** Canonicalize only the compared immutable AST DAGs, preserving sharing and exact syntax.
 *  Reference-keyed traversal avoids recursively hashing types; shallow signatures additionally
 *  identify equal types allocated separately. All metadata belongs to one comparison. */
private class SyntaxDag:
  /** Physical nodes already traversed during this comparison. */
  private val physical = IdentityHashMap[Type, Int]()

  /** Exact syntax classes, indexed without Type equality or Type hashing. */
  private val canonical = mutable.Map.empty[SyntaxNode, Int]

  /** Canonical nodes and their finite AST heights. */
  private val nodes = mutable.ArrayBuffer.empty[(SyntaxNode, Int)]

  /** Index a finite DAG iteratively so deep shared types do not consume the call stack. */
  def index(root: Type): Int =
    var pending = List((root, false))
    while pending.nonEmpty do
      val (type_, ready) = pending.head
      pending = pending.tail
      if !physical.containsKey(type_) then
        if ready then
          val children = type_.components.map(physical.get)
          val node = SyntaxNode(head(type_), children)
          val id = canonical.getOrElseUpdate(node, {
            val height = children.foldLeft(0)((height, child) => height.max(nodes(child)._2 + 1))
            nodes += ((node, height))
            nodes.size - 1
          })
          physical.put(type_, id)
        else
          pending = (type_, true) :: pending
          type_.components.reverseIterator.foreach: child =>
            if !physical.containsKey(child) then pending = (child, false) :: pending
    physical.get(root)

  /** Compare syntax classes by finite DAG reachability, never by a variable's semantic bound. */
  def isSubterm(child: Int, parent: Int): Boolean =
    if child == parent then return true
    val childHeight = nodes(child)._2
    if childHeight >= nodes(parent)._2 then return false
    var pending = nodes(parent)._1.children
    val visited = mutable.BitSet.empty
    while pending.nonEmpty do
      val id = pending.head
      pending = pending.tail
      if id == child then return true
      if !visited(id) && nodes(id)._2 > childHeight then
        visited += id
        pending = nodes(id)._1.children ::: pending
    false

  /** Preserve every scalar field that participates in concrete Type equality. */
  private def head(type_ : Type): SyntaxHead =
    type_ match
      case TBot => SyntaxHead.Bottom
      case TTop => SyntaxHead.Top
      case TVar(variable) => SyntaxHead.Variable(variable.name)
      case TClass(variable) => SyntaxHead.Class(variable.name)
      case TNeg(_) => SyntaxHead.Negation
      case TField(name, _) => SyntaxHead.Field(name)
      case TTuple(_, _) => SyntaxHead.Tuple
      case TLam(_, _) => SyntaxHead.Arrow
      case TJointType(mode, _, _) => SyntaxHead.Joint(mode)
      case TApp(_, _) => SyntaxHead.Application
      case TUniv(variable, _) => SyntaxHead.Universal(variable.name)
      case TConstrained(_, constraint) => SyntaxHead.Guard(constraint.dir)
