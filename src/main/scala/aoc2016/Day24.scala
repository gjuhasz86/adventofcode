package aoc2016

import common.search.ASearch
import common.search.ASearch.State

import scala.io.Source
import scala.collection.immutable.Set

object Day24 extends App {

  case class Board(tiles: Vector[Vector[Boolean]]) {
  }

  type CharBoard = Vector[Vector[Char]]
  implicit class RichBoard(b: CharBoard) {
    def get(r: Int, c: Int): Char = b(r)(c)
    def isOpen(p: Pos): Boolean = get(p.r, p.c) != '#'
    def mark(r: Int, c: Int, ch: Char): CharBoard =
      if (r >= 0 && c >= 0)
        b.updated(r, b(r).updated(c, ch))
      else b
    def mark(p: Pos, ch: Char): CharBoard = mark(p.r, p.c, ch)

    def p: String = b.map(r => r.mkString).mkString("\n")
  }

  type Dir = (Int, Int)
  case class Pos(r: Int, c: Int) {
    def move(dir: Dir): Pos = Pos(r + dir._1, c + dir._2)
    def dist(p: Pos): Int = Math.abs(r - p.r) + Math.abs(c - p.c)
  }

  def rotations[T](list: List[T]): List[List[T]] =
    list.indices.toList.map { i =>
      list.drop(i) ::: list.take(i)
    }

  case class MazeState(board: CharBoard, pos: Pos, goalPos: Pos)(override val cost: Int = 0) extends State {
    override type OutState = MazeState
    override type Move = Pos

    override def moves: List[Pos] = {
      List((1, 0), (0, 1), (-1, 0), (0, -1))
        .map(pos.move)
    }

    override def next(m: Pos): MazeState =
      MazeState(board, m, goalPos)(cost + 1)


    override def valid: Boolean = board.isOpen(pos)
    override def goal: Boolean = pos == goalPos

    override def estToGoal: Int = pos.dist(goalPos)

  }

  val input = Source.fromFile("src/main/resources/aoc2016/day24.txt").getLines().toVector

  val input1 =
    """###########
      |#0.1.....2#
      |#.#######.#
      |#4.......3#
      |###########""".stripMargin.lines.toVector

  def parse(c: Char, row: Int, col: Int): (Boolean, Option[(Char, Pos)]) = c match {
    case '#' => (false, None)
    case '.' => (true, None)
    case n => (true, Some((n, Pos(row, col))))
  }

  val charBoard: CharBoard = input.map(_.toVector)

  val locations = for {
    (row, rowIdx) <- charBoard.zipWithIndex
    (ch, colIdx) <- row.zipWithIndex
    if ch != '.' && ch != '#'
  } yield (ch.toString.toInt, Pos(rowIdx, colIdx))

  locations foreach println

  val distances = for {
    i <- locations.indices
    j <- i + 1 until locations.size
    (a, aPos) = locations(i)
    (b, bPos) = locations(j)
  } yield {
    val res = ASearch.search(MazeState(charBoard, aPos, bPos)(), findAll = false, debug = false).head
    val dist = res.pathLenght
    val markedBoard = res.statePath.foldLeft(charBoard)((b, s) => b.mark(s.pos.r, s.pos.c, 'o'))
    println(markedBoard.p)
    println(s"Found distance: ($a,$b) = $dist")
    List((a, b) -> dist, (b, a) -> dist)
  }

  val distancesMap = distances.flatten.toMap
  distancesMap.toList.sorted foreach println


  case class HamiltonState(visited: List[Int], unvisited: Set[Int])(override val cost: Int) extends State {
    override type OutState = HamiltonState
    override type Move = Int

    override def moves: List[Move] = if (unvisited.nonEmpty) unvisited.toList else List(0)
    override def next(m: Move): HamiltonState = HamiltonState(visited :+ m, unvisited - m)(distancesMap(visited.last, m) + cost)
    override def valid: Boolean = true
    override def goal: Boolean = unvisited.isEmpty && visited.last == 0
    override def estToGoal: Int = 0

    override def toString: String = s"[${visited.mkString(",")}][${unvisited.mkString(",")}] ($cost)"
  }

  val startHam = HamiltonState(0 :: Nil, locations.map(_._1).toSet - 0)(0)
  val results = ASearch.search(startHam, findAll = true, debug = true)
  results.sortBy(_.state.cost).reverse.map(_.state) foreach println
}

// 574 - too high
