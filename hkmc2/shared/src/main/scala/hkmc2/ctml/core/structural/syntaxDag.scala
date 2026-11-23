package hkmc2.ctml.core.structural

import java.util.IdentityHashMap
import scala.collection.mutable

import hkmc2.ctml.types.*

/** Constructor data independent of nested types, suitable for ordinary hashing. */
private enum SyntaxHead:
  case Bottom, Top, Negation, Tuple, Arrow, Application
  case Variable(name: String)
  case Class(name: String)
  case Field(name: String)
  case Joint(mode: JointMode)
  case Universal(name: String)
  case Guard(direction: Direction)
  case Binder
  case Bound(index: Int)

/** The two views share constructor labels and child order. Literal subterm descent also
 *  observes binder names. Alpha-equivalence observes binding positions instead. */
private object SyntaxHead:
  /** Preserve every scalar field that participates in concrete Type equality. */
  def literal(type_ : Type): SyntaxHead =
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

  /** Resolve variables against the nearest enclosing binder. */
  def scoped(type_ : Type, binders: List[TypeVar]): SyntaxHead = type_ match
    case TUniv(_, _) => Binder
    case TVar(variable) =>
      val index = binders.indexOf(variable)
      if index < 0 then Variable(variable.name) else Bound(index)
    case _ => literal(type_)

/** A syntax node with the local indices of its children, in source order. */
private[structural] case class SyntaxNode(head: SyntaxHead, children: List[Int])

/** A syntax DAG for literal subterm queries or for keys modulo bound variable names. */
private[core] object SyntaxDag:
  /** Retain binder names even when comparing a type to an open subterm. */
  def literal(): SyntaxDag = new SyntaxDag(None)

  /** In a single-root DAG, postorder assigns each distinct node its index on first occurrence.
   *  The resulting sequence is independent of physical sharing and uses no recursive hashing. */
  private[structural] def alphaKey(root: Type): Vector[SyntaxNode] =
    val dag = new SyntaxDag(Some(Nil))
    dag.index(root)
    dag.nodes.iterator.map(_._1).toVector

/** Intern shallow signatures iteratively, sharing physical subtrees within their lexical scope. */
private[core] final class SyntaxDag private (binders: Option[List[TypeVar]]):
  /** Physical sharing is meaningful within a scope. The same node may be open in another one. */
  private class Scope(val binders: Option[List[TypeVar]]):
    /** Source nodes already indexed under these bindings. */
    val visited = IdentityHashMap[Type, Int]()

    /** A literal traversal uses one table throughout. Only a universal extends lexical scope. */
    def childrenOf(type_ : Type): Scope = (type_, binders) match
      case (TUniv(variable, _), Some(outer)) => Scope(Some(variable :: outer))
      case _ => this

  /** The initial scope is shared by successive literal subterm queries. */
  private val rootScope = Scope(binders)

  /** Structurally equal nodes, including nodes visited in separate scopes, share an index. */
  private val canonical = mutable.Map.empty[SyntaxNode, Int]

  /** Canonical nodes and their finite AST heights. */
  private val nodes = mutable.ArrayBuffer.empty[(SyntaxNode, Int)]

  /** Visit children before their parent. A pending parent retains its children's scope. */
  def index(root: Type): Int =
    var pending = List((root, rootScope, Option.empty[Scope]))
    while pending.nonEmpty do
      val (type_, scope, ready) = pending.head
      pending = pending.tail
      if !scope.visited.containsKey(type_) then ready match
        case None =>
          val inner = scope.childrenOf(type_)
          pending = (type_, scope, Some(inner)) :: pending
          type_.components.reverseIterator.foreach: child =>
            pending = (child, inner, None) :: pending
        case Some(inner) =>
          val head = scope.binders match
            case None => SyntaxHead.literal(type_)
            case Some(bound) => SyntaxHead.scoped(type_, bound)
          val children = type_.components.map(inner.visited.get)
          val node = SyntaxNode(head, children)
          val id = canonical.getOrElseUpdate(node, {
            val height = children.foldLeft(0)((h, child) => h.max(nodes(child)._2 + 1))
            nodes += ((node, height))
            nodes.size - 1
          })
          scope.visited.put(type_, id)
    rootScope.visited.get(root)

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
