package common

import scala.collection.immutable.Set
import ASearch._


class ASearch[S <: State](stateStrFn: (Set[S], List[S], S) => String)(implicit toState: S#OutState => S) {
  implicit private def nodeOrdering: Ordering[Node] =
    Ordering.by((n: Node) => (n.state.estToGoalCached + n.pathLenght, n.state.estToGoalCached))

  sealed trait Node {
    def state: S
    def createChild(s: S, m: S#Move): Node = ChildNode(s)(m, this)
    def pathLenght: Int
    def path: List[S#Move]
    def statePath: List[S]

    def printPath(): Unit = {
      this match {
        case n: RootNode =>
          println()
          println(n.state)
        case n: ChildNode =>
          n.parent.printPath()
          println()
          println(n.move)
          println(n.state)

      }
    }

    override def hashCode(): Int = state.hashCode()
    override def equals(o: scala.Any): Boolean = o match {
      case n: Node => n.state == this.state
      case _ => false
    }

  }

  case class RootNode(state: S) extends Node {
    override val pathLenght = 0
    override def path: List[S#Move] = Nil
    override def statePath: List[S] = state :: Nil
  }
  object RootNode {
    def of(state: S): Node = RootNode(state)
  }
  case class ChildNode(state: S)(val move: S#Move, val parent: Node) extends Node {
    override val pathLenght: Int = parent.pathLenght + 1
    override def path: List[S#Move] = parent.path :+ move
    override def statePath: List[S] = parent.statePath :+ state
  }

  private def search0(expanded: Set[Node], unexpanded: List[Node]): Option[Node] = unexpanded.headOption match {
    case None => None
    case Some(current) if current.state.goal =>
      val debugStr = stateStrFn(expanded.map(_.state), unexpanded.map(_.state), current.state)
      if (debugStr.nonEmpty) {
        println(s"Step* ${expanded.size}/${unexpanded.size} [${current.pathLenght}+${current.state.estToGoalCached}=${current.pathLenght + current.state.estToGoal}]")
        println(debugStr)
        println()
      }
      Some(current)
    case Some(current) =>
      val debugStr = stateStrFn(expanded.map(_.state), unexpanded.map(_.state), current.state)
      if (debugStr.nonEmpty) {
        println(s"Step* ${expanded.size}/${unexpanded.size} [${current.pathLenght}+${current.state.estToGoalCached}=${current.pathLenght + current.state.estToGoal}]")
        println(debugStr)
        println()
      }
      val nexts = current.state.nextStates.map { case (m, s) => current.createChild(s, m) }
      val uniqNexts = nexts.filterNot(n => expanded.contains(n))
      val unexpTail = unexpanded.drop(1)
      val newUnexpanded = uniqNexts ++ unexpTail
      search0(expanded + current, newUnexpanded.sorted.distinct)
  }


}

object ASearch {

  import scala.collection.mutable

  val heurCache: mutable.Map[Any, Int] = mutable.Map.empty

  def search[S <: State](state: S, stateStrFn: (Set[S], List[S], S) => String = (_: Any, _: Any, c: S) => c.toString)(implicit toState: S#OutState => S): Option[ASearch[S]#Node] = {
    val aSearch = new ASearch[S](stateStrFn)
    implicit val nodeOrdering = Ordering.by((n: aSearch.Node) => n.state.estToGoalCached + n.pathLenght)
    aSearch.search0(Set.empty, List(aSearch.RootNode.of(state)))
  }

  trait State {
    type OutState <: State
    type Move

    def moves: List[Move]
    def next(m: Move): OutState
    def valid: Boolean

    def goal: Boolean
    def estToGoal: Int
    final def estToGoalCached: Int = estToGoal
    final def estToGoalCached0: Int = heurCache.lift(this) match {
      case Some(h) =>
        //        println(s"cache hit [${heurCache.size}]")
        //        println(s"$this")
        //        println(s"---")
        h
      case None =>
        val h = estToGoal
        //        println(s"cache miss [${heurCache.size}]")
        //        println(s"$this")
        //        println(s"---")
        heurCache += (this -> h)
        h
    }

    final def nextStates: List[(Move, OutState)] = {
      val mvs = this.moves
      val ns = mvs.map(m => this.next(m))
      val valids = ns.map(_.valid)
      val x = mvs zip ns zip valids
      mvs.map(m => (m, this.next(m))).filter(_._2.valid)
    }
  }
}
