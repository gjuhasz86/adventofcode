package aoc2016.nope

import java.security.MessageDigest

import common.Utils.timed

object Day17v1 extends App {

  import common.ASearch
  import common.ASearch.State

  case class Pos(r: Int, c: Int)

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  val passcode = "yjjvjgan"
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

    override def moves: List[Dir] =
      (md5(passcode + route) zip dirs).toList
        .collect { case (c, d) if open.contains(c) => d }

    override def next(m: Dir): MazeState = MazeState(m.r + r, m.c + c, route + m.str)

    override def valid: Boolean = r > 0 && c > 0 && r <= goalPos.r && c <= goalPos.c
    override def goal: Boolean = r == goalPos.r && c == goalPos.c
    override def estToGoal: Int = Math.abs(r - goalPos.r) + Math.abs(c - goalPos.c)
  }

  val res = timed {
    ASearch.search(startPos)
  }
  println(res)
}
