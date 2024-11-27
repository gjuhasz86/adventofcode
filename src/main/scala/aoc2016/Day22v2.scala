package aoc2016

import common.search.ASearch
import common.search.ASearch.State

import scala.collection.immutable.Set
import scala.io.Source

object Day22v2 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
  case class Pos(x: Int, y: Int) {

    def dist(p: Pos): Int = Math.abs(x - p.x) + Math.abs(y - p.y)

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
  trait Storage
  case object Fix extends Storage {
    override def toString: String = "#"
  }
  case object Movable extends Storage {
    override def toString: String = "."
  }
  case object Empty extends Storage {
    override def toString: String = "o"
  }

  case class Node(pos: Pos, storage: Storage) {
    def x: Int = pos.x
    def y: Int = pos.y
  }

  case class Grid(nodes: Vector[Storage], maxX: Int, maxY: Int) {
    def switch(from: Pos, to: Pos): Grid = {
      val newNodes = nodes.updated(idx(from), nodes(idx(to)))
        .updated(idx(to), nodes(idx(from)))
      Grid(newNodes, maxX, maxY)
    }

    def idx(p: Pos): Int = p.y * (maxX + 1) + p.x

    def at(p: Pos): Storage = nodes(idx(p))

    def pretty(goalPos: Pos): String = {
      val part = nodes
        .map(_.toString)
        .grouped(maxX + 1)
        .toVector
      part
        .updated(goalPos.y, part(goalPos.y).updated(goalPos.x, "G"))
        .map(_.mkString)
        .mkString("\n")
    }
  }

  val input = Source.fromFile("src/main/resources/aoc2016/day22.txt").getLines().toList

  def parse(str: String): Node = str match {
    case r"/dev/grid/node-x([0-9]+)$x-y([0-9]+)$y +([0-9]+)${size}T +([0-9]+)${used}T +[0-9]+T +[0-9]+%" =>
      val storage =
        if (size.toInt > 500)
          Fix
        else if (used.toInt == 0)
          Empty
        else
          Movable
      Node(Pos(x.toInt, y.toInt), storage)
  }

  val nodes = input.drop(2) map parse

  val nodes1 =
    """/dev/grid/node-x0-y0   10T    8T     2T   80%
      |/dev/grid/node-x0-y1   11T    6T     5T   54%
      |/dev/grid/node-x0-y2   32T   28T     4T   87%
      |/dev/grid/node-x1-y0    9T    7T     2T   77%
      |/dev/grid/node-x1-y1    8T    0T     8T    0%
      |/dev/grid/node-x1-y2   11T    7T     4T   63%
      |/dev/grid/node-x2-y0   10T    6T     4T   60%
      |/dev/grid/node-x2-y1    9T    8T     1T   88%
      |/dev/grid/node-x2-y2    9T    6T     3T   66%""".stripMargin.lines.toList map parse

  nodes foreach println

  case class GridState(grid: Grid, empty: Pos, goalPos: Pos) extends State {
    override type OutState = GridState
    override type Move = Pos

    override def moves: List[Pos] = empty.neighbors(grid.maxX, grid.maxY).filter(p => grid.at(p) != Fix)
    override def next(target: Pos): GridState = {
      val newGoalPos = if (target == goalPos) empty else goalPos
      GridState(grid, target, newGoalPos)
    }
    override def valid: Boolean = true
    override def goal: Boolean = goalPos == Pos(0, 0)
    override def cost: Int = 1
    override def estToGoal: Int = goalPos.x + goalPos.y + goalPos.dist(emptyPos) - 1
    override def toString: String = grid.pretty(goalPos)
  }

  val maxX = nodes.filter(_.pos.y == 0).maxBy(_.pos.x).pos.x
  val maxY = nodes.filter(_.pos.x == 0).maxBy(_.pos.y).pos.y
  val sortedNodes = nodes.toVector.sortBy(_.pos.tup.swap)
  val emptyPos = sortedNodes.find(_.storage == Empty).get.pos
  val storages = sortedNodes.map(_.storage)
  val grid = Grid(storages, maxX, maxY)
  println(grid.pretty(Pos(maxX, 0)))

  type Board = Vector[Vector[Char]]
  implicit class RichBoard(b: Board) {
    def mark(r: Int, c: Int, ch: Char): Board =
      if (r >= 0 && c >= 0)
        b.updated(r, b(r).updated(c, ch))
      else b
    def mark(p: Pos, ch: Char): Board = mark(p.y, p.x, ch)

    def p: String = b.map(r => r.mkString).mkString("\n")
  }

  def debugState(exp: Set[Pos], unexp: List[Pos]): String = {
    val b1 = exp.foldLeft(board)((b, p) => b.mark(p, '+'))
    val b2 = unexp.foldLeft(b1)((b, p) => b.mark(p, '?'))
    b2.mark(unexp.head, 'o').p
  }

  val board: Board = grid.nodes.map(_.toString.head).grouped(grid.maxX + 1).toVector
  val debugStr: (Set[GridState], List[ASearch.Node[GridState]]) => String = (expanded, unexpanded) =>
    debugState(expanded.map(_.empty), unexpanded.map(_.state.empty))

  val result = ASearch.search(GridState(grid, emptyPos, Pos(maxX, 0)), false, true, debugStr).head
  result.printPath()
  println(result.pathLenght)
}
