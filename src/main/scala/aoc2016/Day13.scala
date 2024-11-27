package aoc2016

import common.ASearch
import common.ASearch.State

object Day13 extends App {

  def isOpen(z: Int)(c: Int, r: Int): Boolean = {
    def isOpen0: Boolean = {
      val a = c * c + 3 * c + 2 * c * r + r + r * r
      val b = a + z
      b.toBinaryString.count(_ == '1') % 2 == 0
    }
    if (c < 0 || r < 0) false else isOpen0
  }

  def maze(mr: Int, mc: Int): Board = (for {
    r <- 0 until mr
    c <- 0 until mc
  } yield if (isOpen(favNum)(c, r)) ' ' else ':').grouped(mc).toIndexedSeq

  val favNum = 1352
  val startPos = Pos(1, 1)
  val goalPos = Pos(39, 31)
  val subMaze = maze(50, 50)
  println(isOpen(favNum)(goalPos.c, goalPos.r))

  type Dir = (Int, Int)
  case class Pos(r: Int, c: Int) extends State {
    override type OutState = Pos
    override type Move = Dir

    override def moves: List[Dir] = List((1, 0), (0, 1), (-1, 0), (0, -1))
    override def next(m: Dir): Pos = Pos(m._1 + r, m._2 + c)

    override def valid: Boolean = isOpen(favNum)(c, r)
    override def goal: Boolean = this == goalPos
//    override def estToGoal: Int = Math.abs(r - goalPos.r) + Math.abs(c - goalPos.c)
    override def estToGoal: Int = 0

    override def toString: String = subMaze.mark(r, c, 'o').p
  }

  type Board = IndexedSeq[IndexedSeq[Char]]
  implicit class RichBoard(b: Board) {
    def mark(r: Int, c: Int, ch: Char): Board =
      if (r >= 0 && c >= 0)
        b.updated(r, b(r).updated(c, ch))
      else b
    def mark(p: Pos, ch: Char): Board = mark(p.r, p.c, ch)

    def p: String = b.map(r => r.mkString).mkString("\n")
  }

  def debugState(exp: Set[Pos], unexp: List[Pos], curr: Pos): String = {
    val b1 = exp.foldLeft(subMaze)((b, p) => b.mark(p, '+'))
    val b2 = unexp.foldLeft(b1)((b, p) => b.mark(p, '#'))
    b2.mark(curr, 'o').p
  }

  val res1 = ASearch.search(startPos, debugState).get
  val res1Str = res1.statePath.foldLeft(maze(55, 55))((b, p) => b.mark(p.r, p.c, 'o'))
  val resPart1 = res1.pathLenght

  println()
  println(res1Str.p)
  println(resPart1)
  println()


  case class Pos2(r: Int, c: Int)(val step: Int) {
    def moves: List[Dir] = List((1, 0), (0, 1), (-1, 0), (0, -1))
    def next(m: Dir): Pos2 = Pos2(m._1 + r, m._2 + c)(step + 1)
    def valid: Boolean = isOpen(favNum)(c, r)
  }
  def findAll(expanded: Set[Pos2], unexpanded: List[Pos2]): Set[Pos2] = {
    unexpanded match {
      case Nil => expanded
      case h :: rest =>
        println(s"[${h.step}] ${expanded.size}/${unexpanded.size}")
        val nexts = h.moves.map(h.next).filter(_.valid).filter(_.step <= 50) ++ rest
        val uniqNexts = nexts.filterNot(expanded.contains)
        findAll(expanded + h, uniqNexts.sortBy(_.step).distinct)
    }
  }
  val res = findAll(Set.empty, List(Pos2(1, 1)(0)))

  val rStr = res.foldLeft(maze(55, 55))((b, p) => b.mark(p.r, p.c, p.step.toString.last))
  println(rStr.p)

  println(res)
  println(res.size)

  val resPart2 = res.size

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")
}
