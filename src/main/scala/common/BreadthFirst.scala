package common

import BreadthFirst._

import scala.collection.parallel.immutable.ParSeq

class BreadthFirst[S <: State](val findAll: Boolean)(implicit toState: S#OutState => S) {

  private def search0(unexpanded: ParSeq[S]): List[S] = unexpanded match {
    case u if u.isEmpty => Nil
    case u if !findAll && u.exists(_.goal) => u.filter(_.goal).toList
    case u =>
      val newUnexpanded: ParSeq[S] = u.flatMap {
        case s if s.goal => println(s"FOUND: $s"); Nil
        case s => s.nexts.map(toState)
      }
      search0(newUnexpanded)
  }
}

object BreadthFirst {

  def search[S <: State](state: S, allGoals: Boolean = false)(implicit toState: S#OutState => S): List[S] = {
    new BreadthFirst[S](allGoals).search0(List(state).par)
  }


  trait State {
    type OutState <: State
    type Move

    def moves: List[Move]
    def next(m: Move): OutState
    def valid: Boolean

    def goal: Boolean

    final def nexts: List[OutState] = moves.map(next).filter(_.valid)
  }

}
