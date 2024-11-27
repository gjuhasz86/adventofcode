package common.search

import scala.collection.immutable.Set
import ASearch.State


class ASearch[S <: State](findAll: Boolean, debug: Boolean, debugN: Int, debugStr: (Set[S], List[ASearch.Node[S]]) => String)(implicit toState: S#OutState => S) {
  final type Node = ASearch.Node[S]

  private def search0(goals: List[Node], expanded: Set[S], unexpanded: List[Node]): List[Node] =
    unexpanded.headOption match {
      case None => goals
      case Some(node) if node.state.goal && findAll =>
        if (debug && expanded.size % debugN == 0) println(s"Step* ${expanded.size}/${unexpanded.size} [${node.pathLenght}+${node.state.estToGoal}=${node.pathLenght + node.state.estToGoal}]")
        //        println(debugStr(expanded, unexpanded))
        println(s"FOUND: ${node.state}")
        search0(node :: goals, expanded + node.state, unexpanded.tail)
      case Some(node) if node.state.goal => List(node)
      case Some(current) =>
        if (debug && expanded.size % debugN == 0) println(s"Step* ${expanded.size}/${unexpanded.size} [${current.pathLenght}+${current.state.estToGoal}=${current.pathLenght + current.state.estToGoal}]")
        //        println(debugStr(expanded, unexpanded))
        val nexts = current.state.nextStates.collect {
          case (m, s) if !expanded.contains(s) => current.createChild(s, m)
        }
        val newUnexpanded = unexpanded.drop(1) ++ nexts
        search0(goals, expanded + current.state, newUnexpanded.distinct.sorted)
    }
}

object ASearch {
  def search[S <: State](start: S,
                         findAll: Boolean = false,
                         debug: Boolean = false,
                         debugStr: (Set[S], List[ASearch.Node[S]]) => String = (_: Set[S], _: List[ASearch.Node[S]]) => "",
                         debugN: Int = 1
                        )(
                          implicit toState: S#OutState => S): List[ASearch[S]#Node] =
    new ASearch[S](findAll, debug, debugN, debugStr).search0(Nil, Set.empty, List(RootNode(start)))

  trait State {
    type OutState <: State
    type Move

    def moves: List[Move]
    def next(m: Move): OutState
    def valid: Boolean

    def goal: Boolean
    def cost: Int
    def estToGoal: Int

    final def nextStates: List[(Move, OutState)] = {
      val mvs = this.moves
      val ns = mvs.map(m => this.next(m))
      val valids = ns.map(_.valid)
      val x = mvs zip ns zip valids
      mvs.map(m => (m, this.next(m))).filter(_._2.valid)
    }
  }

  object Node {
    implicit def nodeOrdering[S <: State]: Ordering[Node[S]] = Ordering.by(n => n.state.cost + n.state.estToGoal)
  }

  sealed trait Node[S <: State] {
    def state: S
    def createChild(s: S, m: S#Move): Node[S] = ChildNode(s)(m, this)
    def pathLenght: Int
    def path: List[S#Move]
    def statePath: List[S]

    def printPath(): Unit = {
      this match {
        case RootNode(state) =>
          println()
          println(state)
        case n@ChildNode(state) =>
          n.parent.printPath()
          println()
          println(n.move)
          println(n.state)

      }
    }

    override def hashCode(): Int = state.hashCode()
    override def equals(o: scala.Any): Boolean = o match {
      case n: Node[_] => n.state == this.state
      case _ => false
    }

  }

  case class RootNode[S <: State](state: S) extends Node[S] {
    override val pathLenght = 0
    override def path: List[S#Move] = Nil
    override def statePath = List(state)
  }
  object RootNode {
    def of[S <: State](state: S): Node[S] = RootNode(state)
  }
  case class ChildNode[S <: State](state: S)(val move: S#Move, val parent: Node[S]) extends Node[S] {
    override val pathLenght: Int = parent.pathLenght + 1
    override def path: List[S#Move] = parent.path :+ move
    override def statePath = state :: parent.statePath
  }
}
