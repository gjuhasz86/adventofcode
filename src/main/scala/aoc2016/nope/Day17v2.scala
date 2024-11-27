package aoc2016.nope

import java.security.MessageDigest

import common.Utils.timed

// guessed 100 - too low
// guessed 500 - too high
object Day17v2 extends App {

  import common.BreadthFirst
  import common.BreadthFirst.State

  case class Pos(r: Int, c: Int)

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  //  val passcode = "yjjvjgan"
  val passcode = "ihgpwlah"
  val startPos = MazeState(1, 1, "")
  val goalPos = Pos(4, 4)

  val open = "bcdef"
  sealed abstract class Dir(val r: Int, val c: Int, val str: String)
  case object Up extends Dir(-1, 0, "U")
  case object Down extends Dir(1, 0, "D")
  case object Left extends Dir(0, -1, "L")
  case object Right extends Dir(0, 1, "R")
  val dirs: List[Dir] = List(Up, Down, Left, Right)

  case class MazeState(r: Int, c: Int, route: String) extends State {
    override type OutState = MazeState
    override type Move = Dir

    override def moves: List[Dir] = {
      val hash = md5(passcode + route)
      val r0 = (hash zip dirs).toList
      val r1 = r0.collect { case (ch, d) if open.contains(ch) => d }
      r1
    }

    override def next(m: Dir): MazeState = MazeState(m.r + r, m.c + c, route + m.str)

    override def valid: Boolean = r > 0 && c > 0 && r <= goalPos.r && c <= goalPos.c
    override def goal: Boolean = r == goalPos.r && c == goalPos.c

    override def toString: String = s"[${route.length}] [$r,$c] [$route]"
  }
  val res = timed {
    BreadthFirst.search(startPos, true)
  }
  println(res)
}
