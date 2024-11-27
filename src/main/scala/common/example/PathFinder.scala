package common.example

import common.ASearch
import common.ASearch.State

object PathFinder extends App {

  type Board = List[List[Char]]

  object Board {
    def of(r: Int, c: Int): Board = (1 to r).toList.map(_ => (1 to c).toList.map(_ => '.'))
  }
  implicit class RichBoard(b: Board) {
    def mark(r: Int, c: Int, ch: Char): Board =
      b.updated(r, b(r).updated(c, ch))
    def mark(p: Pos, ch: Char): Board = mark(p.r, p.c, ch)

    def rows = b.size
    def cols = b.head.size
    def p: String = b.map(r => r.mkString).mkString("\n")
  }

  type Dir = (Int, Int)

  case class Pos(r: Int, c: Int) extends State {
    override type OutState = Pos
    override type Move = Dir
    override def moves: List[Dir] = (0, 1) :: (1, 0) :: (0, -1) :: (-1, 0) :: Nil
    override def next(m: Dir): Pos = Pos(r + m._1, c + m._2)
    override def valid: Boolean = r >= 0 && c >= 0 && r < board.rows && c < board.cols && board(r)(c) == '.'
    override def goal: Boolean = this == goalPos
    override def estToGoal: Int = {
      val res = Math.abs(r - goalPos.r) + Math.abs(c - goalPos.c)
//      println(s"from (${this.r},${this.c}) to (${goalPos.r},${goalPos.c}): $res")
      res
    }
    override def toString: String = board.mark(this, 'o').p
  }


  val toBool = (c: Char) => if (c == '.') true else false
  val board =
    """...............
      |.x.....xxxxx...
      |.x..x.x.....xxx
      |....xx....xx...
      |...............""".stripMargin.lines.toList.map(_.toList)

  println(board.p)
  println(board.rows)
  println(board.cols)

  val startPos = Pos(4, 14)
  val goalPos = Pos(1, 14)
  println(board.mark(startPos, 'o').p)
  println()

  def debugState(exp: Set[Pos], unexp: List[Pos], curr: Pos): String = {
    val b1 = exp.foldLeft(board)((b, p) => b.mark(p, '+'))
    val b2 = unexp.foldLeft(b1)((b, p) => b.mark(p, '*'))
    b2.mark(curr, 'o').p
  }

  val res = ASearch.search(startPos, debugState)
  res.get.printPath()
}
