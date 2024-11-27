package aoc2016

import java.security.MessageDigest

import common.BreadthFirst

object Day17 extends App {

  import common.search.ASearch
  import common.search.ASearch.State

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

  case class MazeState(r: Int, c: Int, route: String) extends State with BreadthFirst.State {
    override type OutState = MazeState
    override type Move = Dir

    override def moves: List[Dir] =
      (md5(passcode + route) zip dirs).toList
        .collect { case (c, d) if open.contains(c) => d }

    override def next(m: Dir): MazeState = MazeState(m.r + r, m.c + c, route + m.str)

    override def valid: Boolean = r > 0 && c > 0 && r <= goalPos.r && c <= goalPos.c
    override def goal: Boolean = r == goalPos.r && c == goalPos.c
    override def estToGoal: Int = 0
    override val cost: Int = route.length

    override def toString: String = s"[${route.length}] [$r,$c] [$route]"
  }

  val part1Res = ASearch.search(startPos, false, true).head
  val part2Res = ASearch.search(startPos, true, false).head
  println()
  println(s"PART1: $part1Res")
  println(s"PART1: $part2Res")
}