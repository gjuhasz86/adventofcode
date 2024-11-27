package aoc2017

import math._

object Day03 {

  import ShowInstances._

  type Vec[T] = Vector[T]
  type Vec2d[T] = Vec[Vec[T]]

  implicit def vec2dShow[T: Show] = new Show[Vec2d[T]] {
    override def show(t: Vec2d[T]): String =
      t.map(r => r.map(_.show).mkString("\t")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
//    val starts: Seq[Int] = (0 to 10).map(ringMax)
//    starts.pp
//    "".pp
//    gen((-3, -3), (3, 3))(x => ex1(x.toIdx)).pp
//    gen((-1, -1), (1, 1))(identity).pp
//    "".pp

    val in = 265149
    ex1(in).pp
    ex2(in).pp
  }

  def ex1(in: Int) = {
    toPos(in - 1) dist ((0, 0))
  }

  def ex2(in: Int) = {
    def find(vec: Vec[Int]): Vec[Int] = {
      val nVec = next(vec)
      if (nVec.last > in)
        nVec
      else
        find(nVec)
    }

    find(Vector(1)): Seq[Int]
  }

  def next(vec: Vec[Int]): Vec[Int] = {
    val i = vec.size // the next index to calculate

    val sum =
      toPos(i) // get the corresponding position
        .adjs // get the adjacent positions
        .map(_.toIdx) // convert all adjacent positions to indices
        .filter(_ < i) // keep only the ones already calculated
        .map(vec.apply) // get the calculated values
        .sum

    vec :+ sum
  }

  val test = (p: Pos) => toPos(p.toIdx) == p

  def toPos(n: Int) = {
    val s = sqrt(n).toInt
    val rIdx = (s + (s % 2)) / 2 // ring index
    val side = rIdx * 2 + 1
    val rd = ringMin(rIdx)
    val diff = n - rd // diff to the ring's base num

    val sd = side - 1
    val u = min(max(diff - (sd * 0), 0), sd)
    val l = min(max(diff - (sd * 1), 0), sd)
    val d = min(max(diff - (sd * 2), 0), sd)
    val r = min(max(diff - (sd * 3), 0), sd)

    (rIdx - l + r, rIdx - u + d)
  }

  def gen[T](bl: Pos, tr: Pos)(f: Pos => T) = {
    (bl.y to tr.y).toVector.map { y =>
      (bl.x to tr.x).toVector.map { x =>
        f((x, y))
      }
    }
  }

  def ringMax(i: Int) = pow(i * 2 + 1, 2).toInt - 1
  def ringMin(i: Int) = pow(i * 2 - 1, 2).toInt - 1

  def dirCode(p: Pos) = {
    val m = p.m
    p match {
      case (x, y) if y == m => 3
      case (x, y) if x == -m => 2
      case (x, y) if y == -m => 1
      case (x, y) if x == m => 0
      case _ => throw new IllegalStateException(s"WAT? [$p]")
    }
  }

  type Pos = (Int, Int)
  implicit class RichPos(val self: Pos) extends AnyVal {
    def x = self._1
    def y = self._2
    def m = max(abs(x), abs(y))

    def dc = dirCode(self)
    def sc = ((4 - self.dc) * 2 - 5).signum
    def r = pow(m * 2 - sc, 2).toInt - 1
    def d = (m, m) dist self

    def toIdx = r + d * sc

    def +(p: Pos): Pos = (x + p.x, y + p.y)
    def -(p: Pos): Pos = (x - p.x, y - p.y)
    def dist(p: Pos): Int = abs(x - p.x) + abs(y - p.y)

    def adjs = adjDirs.map(_ + self)
  }

  val adjDirs = Vector(
    (-1, -1), (0, -1), (1, -1),
    (-1, 0), (1, 0),
    (-1, 1), (0, 1), (1, 1))

  trait Show[T] {
    def show(t: T): String
  }

  object ShowInstances {
    def ofToString[T]: Show[T] = (t: T) => t.toString
    implicit val stringShow: Show[String] = ofToString[String]
    implicit val intShow: Show[Int] = ofToString[Int]
    implicit val boolShow: Show[Boolean] = ofToString[Boolean]
    implicit def tupShow[A, B]: Show[(A, B)] = ofToString[(A, B)]
    implicit def seqShow[T]: Show[Seq[T]] = ofToString[Seq[T]]
  }

  implicit class ShowSyntax[T](self: T)(implicit s: Show[T]) {
    def show: String = s.show(self)
  }

  implicit class AnyPrint[T](self: T)(implicit ev: Show[T]) {
    def pp: Unit = println(self.show)
  }
}