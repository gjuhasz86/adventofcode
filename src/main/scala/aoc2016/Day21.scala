package aoc2016

import scala.io.Source

object Day21 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val input = Source.fromFile("src/main/resources/aoc2016/day21.txt").getLines().toList
  val input0 = List(
    "swap position 4 with position 0",
    "swap letter d with letter b",
    "reverse positions 0 through 4",
    "rotate left 1 step",
    "move position 1 to position 4",
    "move position 3 to position 0",
    "rotate based on position of letter b",
    "rotate based on position of letter d"
  )

  def parse(str: String): Op = str match {
    case r"swap position (\d+)$p1 with position (\d+)$p2" => SwapPos(p1.toInt, p2.toInt)
    case r"swap letter (.)$c1 with letter (.)$c2" => SwapLet(c1.head, c2.head)
    case r"reverse positions (\d+)$p1 through (\d+)$p2" => Reverse(p1.toInt, p2.toInt)
    case r"rotate left (\d+)$n step.?" => RotateLeftN(n.toInt)
    case r"rotate right (\d+)$n step.?" => RotateRightN(n.toInt)
    case r"move position (\d+)$p1 to position (\d+)$p2" => Move(p1.toInt, p2.toInt)
    case r"rotate based on position of letter (.)$c" => RotateLet(c.head)
  }

  def ops = input map parse

  val password1 = "abcdefgh"
  val password2 = "fbgdceah"

  sealed trait Op {
    def fwd(str: String): String
    def bck(str: String): String
  }
  case class SwapPos(p1: Int, p2: Int) extends Op {
    override def fwd(str: String): String = {
      val dropped = str.take(p1) + str(p2) + str.drop(p1 + 1)
      val res = dropped.take(p2) + str(p1) + dropped.drop(p2 + 1)
      res
    }
    override def bck(str: String): String = fwd(str)
  }
  case class SwapLet(c1: Char, c2: Char) extends Op {
    override def fwd(str: String): String = {
      val p1 = str.indexOf(c1)
      val p2 = str.indexOf(c2)
      SwapPos(p1, p2).fwd(str)
    }
    override def bck(str: String): String = fwd(str)
  }
  case class Reverse(p1: Int, p2: Int) extends Op {
    override def fwd(str: String): String = {
      str.take(p1) + str.slice(p1, p2 + 1).reverse + str.drop(p2 + 1)
    }
    override def bck(str: String): String = fwd(str)
  }
  case class RotateLeftN(n0: Int) extends Op {
    override def fwd(str: String): String = {
      val n = n0 % str.length
      str.drop(n) + str.take(n)
    }
    override def bck(str: String): String = RotateRightN(n0 % str.length).fwd(str)
  }
  case class RotateRightN(n: Int) extends Op {
    override def fwd(str: String): String = {
      RotateLeftN(n).fwd(str.reverse).reverse
    }
    override def bck(str: String): String = RotateLeftN(n).fwd(str)
  }
  case class Move(p1: Int, p2: Int) extends Op {
    override def fwd(str: String): String = {
      val dropped = str.take(p1) + str.drop(p1 + 1)
      dropped.take(p2) + str(p1) + dropped.drop(p2)
    }
    override def bck(str: String): String = Move(p2, p1).fwd(str)
  }
  case class RotateLet(c: Char) extends Op {
    override def fwd(str: String): String = {
      val idx = str.indexOf(c)
      val n = if (idx < 4) idx + 1 else idx + 2
      RotateRightN(n).fwd(str)
    }
    override def bck(str: String): String = {
      val originals = (0 to str.length).map { i => RotateLeftN(i).fwd(str) }
      val x = originals.filter(original => RotateLet(c).fwd(original) == str)
      println(x)
      x.head
    }
  }


  assert(SwapPos(4, 0).fwd("abcde") == "ebcda")
  assert(SwapLet('d', 'b').fwd("ebcda") == "edcba")
  assert(Reverse(0, 4).fwd("edcba") == "abcde")
  assert(RotateLeftN(1).fwd("abcde") == "bcdea")
  assert(Move(1, 4).fwd("bcdea") == "bdeac")
  assert(Move(3, 0).fwd("bdeac") == "abdec")
  assert(RotateLet('b').fwd("abdec") == "ecabd")
  assert(RotateLet('d').fwd("ecabd") == "decab")

  assert(SwapPos(4, 0).bck("ebcda") == "abcde")
  assert(SwapLet('d', 'b').bck("edcba") == "ebcda")
  assert(Reverse(0, 4).bck("abcde") == "edcba")
  assert(RotateLeftN(1).bck("bcdea") == "abcde")
  assert(Move(1, 4).bck("bdeac") == "bcdea")
  assert(Move(3, 0).bck("abdec") == "bdeac")
  assert(RotateLet('b').bck("ecabd") == "abdec")
  assert(RotateLet('d').bck("decab") == "ecabd")

  val resPart1 = ops.foldLeft(password1)((s, op) => {
    println(s"$s | $op")
    op.fwd(s)
  })
  println(resPart1)
  val resPart2 = ops.reverse.foldLeft(password2)((s, op) => {
    println(s"$s | $op")
    op.bck(s)
  })
  println(resPart2)

  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")
}
