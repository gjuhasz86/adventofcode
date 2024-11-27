package common


object Search {
  trait State {
    type Self
    def expand: List[Self]
    def goal: Boolean
  }
  def find0[S <: State](start: S)(implicit ev: S#Self <:< S): Option[Node[S]] = {
    val startNode: Node[S] = start :: Nil
    find[S](Nil, startNode :: Nil)
  }

  type Node[S] = List[S]

  def find[S <: State](visited: List[S], unvisited: List[Node[S]])(implicit ev: S#Self => S): Option[Node[S]] = {
    implicit def ev2(list: List[S#Self]): List[S] = list.map(x => ev(x))
    unvisited match {
      case Nil => None
      case (path :+ head) :: tail =>
        println(s"Step ${visited.size}/${unvisited.size} [${path.size}]")
        if (head.goal) {
          Some(path :+ head)
        } else {
          val newVisited = (head :: visited).distinct
          val expandedStates = ev2(head.expand) diff newVisited
          val expandedNodes: List[Node[S]] = expandedStates.map(s => (path :+ head :+ s))
          val newUnvisited = (tail ::: expandedNodes)
          find(newVisited, newUnvisited)
        }
    }
  }
}
