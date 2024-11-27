package aoc2016

import common.search.ASearch
import common.search.ASearch.State

import scala.io.Source

object Day22 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
  case class Pos(x: Int, y: Int) {
    val tup = (x, y)
    def isNextTo(p: Pos): Boolean = {
      val xDist = Math.abs(x - p.x)
      val yDist = Math.abs(y - p.y)
      (xDist == 1 && yDist == 0) || (xDist == 0 && yDist == 1)
    }

    def neighbors(maxX: Int, maxY: Int): List[Pos] = {
      val allPos = List(
        Pos(x + 1, y),
        Pos(x - 1, y),
        Pos(x, y + 1),
        Pos(x, y - 1))
      allPos.filter(p => p.x >= 0 && p.y >= 0 && p.x <= maxX && p.y <= maxY)
    }
    override def toString: String = s"($x,$y)"

  }
  case class Storage(size: Int, used: Int) {
    val avail = size - used
    override def toString: String = if (used == 0)
      f" _ /$size%3d"
    else
      f"$used%3d/$size%3d"
  }
  case class Node(pos: Pos, storage: Storage) {
    val x = pos.x
    val y = pos.y
    val size = storage.size
    val used = storage.used
    val avail = storage.avail

    def deleted: Node = this.copy(storage = storage.copy(used = 0))
    def filled(data: Int): Node = this.copy(storage = storage.copy(used = storage.used + data))
  }

  case class Grid(nodes: Vector[Node], viable: Vector[(Pos, Pos)]) {

    val maxX = nodes.maxBy(_.pos.x).x
    val maxY = nodes.maxBy(_.pos.y).y

    def get(ns: Vector[Node])(p: Pos): Storage = ns.find(n => n.x == p.x && n.y == p.y).get.storage
    def canMove(ns: Vector[Node])(from: Pos, to: Pos): Boolean = {
      val res = get(ns)(from).used <= get(ns)(to).avail && get(ns)(from).used != 0
      res
    }

    def move(from: Pos, to: Pos): Grid = {
      val fromNode = nodes.find(n => n.pos == from).get
      val toNode = nodes.find(n => n.pos == to).get

      val newNodes = fromNode.deleted +: toNode.filled(fromNode.used) +:
        nodes.filterNot(n => n.pos == from || n.pos == to)

      val newViables0 = viable.filterNot { case (f, t) => f == from || f == to || t == from || t == to }
      val fromNeigbors = from.neighbors(maxX, maxY).collect { case p if canMove(newNodes)(p, from) => (p, from) }
      val toNeigbors1 = to.neighbors(maxX, maxY).collect { case p if canMove(newNodes)(p, to) => (p, to) }
      val toNeigbors2 = to.neighbors(maxX, maxY).collect { case p if canMove(newNodes)(to, p) && p != from => (to, p) }
      val newViables = newViables0 ++ fromNeigbors ++ toNeigbors1 ++ toNeigbors2

      //      println("f :" + fromNeigbors)
      //      println("t1:" + toNeigbors1)
      //      println("t2:" + toNeigbors2)

      new Grid(newNodes, newViables)
    }
  }
  object Grid {
    def apply(nodes: Vector[Node]): Grid = {
      val viablePoss =
        viable(nodes)
          .filter { case (a, b) => a.pos.isNextTo(b.pos) }
          .map { case (a, b) => (a.pos, b.pos) }
      new Grid(nodes, viablePoss)
    }
  }

  val input = Source.fromFile("src/main/resources/aoc2016/day22.txt").getLines().toList

  def parse(str: String): Node = str match {
    case r"/dev/grid/node-x([0-9]+)$x-y([0-9]+)$y +([0-9]+)${size}T +([0-9]+)${used}T +[0-9]+T +[0-9]+%" =>
      Node(Pos(x.toInt, y.toInt), Storage(size.toInt, used.toInt))
  }

  val nodes = input.drop(2) map parse

  nodes foreach println

  def viable(ns: Vector[Node]): Vector[(Node, Node)] = for {
    a <- ns
    b <- ns
    if a != b
    if a.used != 0
    if b.avail >= a.used
  } yield (a, b)

  println()
  //  viable foreach println
  //  println(viable.size)


  val nodesTest =
    """/dev/grid/node-x0-y0   10T    8T     2T   80%
      |/dev/grid/node-x0-y1   11T    6T     5T   54%
      |/dev/grid/node-x0-y2   32T   28T     4T   87%
      |/dev/grid/node-x1-y0    9T    7T     2T   77%
      |/dev/grid/node-x1-y1    8T    0T     8T    0%
      |/dev/grid/node-x1-y2   11T    7T     4T   63%
      |/dev/grid/node-x2-y0   10T    6T     4T   60%
      |/dev/grid/node-x2-y1    9T    8T     1T   88%
      |/dev/grid/node-x2-y2    9T    6T     3T   66%""".stripMargin.lines.toList map parse

  //  nodesTest foreach println

  case class Dir(x: Int, y: Int)

  case class GridMove(from: Pos, to: Pos)
  case class GridState(grid: Grid, goalPos: Pos) extends State {
    override type OutState = GridState
    override type Move = GridMove
    override def moves: List[GridMove] = {
      //      val res = viable(grid.nodes)
      //        .filter { case (a, b) => a.pos.isNextTo(b.pos) }
      //        .map { case (a, b) => GridMove(a.pos, b.pos) }
      //      res
      grid.viable
        .map { case (a, b) => GridMove(a, b) }
        .toList
    }
    override def next(m: GridMove): GridState = {
      //      val fromNode = grid.nodes.find(n => n.pos == m.from).get
      //      val toNode = grid.nodes.find(n => n.pos == m.to).get
      //
      //      val res = fromNode.deleted :: toNode.filled(fromNode.used) ::
      //        grid.nodes.filterNot(n => n.pos == m.from || n.pos == m.to)
      //      GridState(Grid(res), newGoalPos)
      val newGoalPos = if (m.from == goalPos) m.to else goalPos
      GridState(grid.move(m.from, m.to), newGoalPos)
    }
    override def goal: Boolean = goalPos == Pos(0, 0)
    override def valid: Boolean = true
    override def cost: Int = 1
    override def estToGoal: Int = goalPos.x + goalPos.y

    override def toString: String = {
      grid.viable + "\n" +
        //        grid.nodes.sortBy(_.pos.tup) + "\n" +
        grid.nodes
          .sortBy(_.pos.tup.swap)
          .map {
            case node if node.pos == goalPos => s"[${node.storage}]"
            case node => s" ${node.storage} "
          }
          .grouped(grid.maxX + 1)
          .map(_.mkString("\t\t"))
          .mkString("\n")
    }
  }

  def part2(cNodes: Vector[Node]): Unit = {

    val goalPos = cNodes.map(_.pos).filter(_.y == 0).maxBy(_.x)
    val startState = GridState(Grid(cNodes), goalPos)
    println(startState)
    //    val result = ASearch.search(startState, false, true)
    //    result.head.printPath()
    //
    //    println(result.head.pathLenght)
  }

  val sizes = nodes.map(_.storage.size).sorted.distinct
  println(sizes)
  val useds = nodes.map(_.storage.used).sorted.distinct
  println(useds)
  val avails = nodes.map(_.storage.avail).sorted.distinct
  println(avails)

  part2(nodes.toVector)
  //  val goalPos = nodesTest.map(_.pos).filter(_.y == 0).maxBy(_.x)
  //  val startState = GridState(Grid(nodesTest), goalPos)
  //  println(startState)
  //  println(startState.next(GridMove(Pos(1, 0), Pos(1, 1))))
}
