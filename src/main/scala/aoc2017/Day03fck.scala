package aoc2017

import scala.math._

object Day03fck {

  def main(args: Array[String]): Unit = {
    val in = 265149

//    test1()
//    val res1 = ex1(in)
//    println(res1)

//    testIdx()
//    testPos()

    val res2 = ex2(in)
    println(res2)
  }

  def test1() = {
    val ins = Vector(4, 11, 16, 23, 26)
    ins.foreach { in => println(s"$in: ${ ex1(in) }") }
  }

  def testIdx() = {
    val ins = Vector(
      (-3, -1), (-3, 2), (1, -3),
      (3, -1), (3, 2), (1, 3))
    ins.foreach { in => println(s"$in: ${ idx(in._1, in._2) }") }
  }

  def testPos() = {
    val idxs = Vector(0, 1, 2, 3, 4, 24, 25, 26, 29, 30, 31, 35, 36, 41, 42)
    idxs.foreach { idx => println(s"$idx: ${ dir(idx) }") }
  }

  def ex1(in: Long): Long = {
    val s = math.sqrt(in).toLong
    val ring = s + (s % 2 - 1)
    val n0 = (ring * ring) + 1 + (ring / 2)
    val ns = (0 to 3).map(i => n0 + (ring + 1) * i)
    val rn = ns.map(i => math.abs(i - in)).min
    val dr = (ring / 2) + 1
    val d = dr + rn
    println(s"DBG $in: $ring, $ns,  $rn, $dr, $d")
    d
  }

  def ex2(in: Long) = {
    val x =
      Stream.from(0)
        .map(i => dir(i))
        .scanLeft(((0L, 0L), 0L)) { case ((pos, i), dir) => (pos + dir, i + 1) }
        .scanLeft(Vector[((Long, Long), Long)]())((acc, x) => acc :+ x)
        .map(_.drop(1))
        .drop(2)
        .scanLeft(Vector[Long](1)) { case (acc, vec) =>
          val (pos, i) = vec.last
          val r = next(pos, i, acc)
          acc :+ r
        }
        .takeWhile(x => x.dropRight(1).lastOption.getOrElse(0L) < in)
        .toVector

//    val n1 = next((0, 1), 1, Vector(1))
    println(x)
    x.last.last
  }

  def next(xy: (Long, Long), i: Long, vec: Vector[Long]): Long = {
    val adjs =
      Vector((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))
        .map { case (x, y) => (x.toLong, y.toLong) }
    val res = adjs.map(_ + xy).map(idx).filter(_ < i)
      .map(i => vec.apply(i.toInt)).sum
//    println(s"DBG ($xy, $i, $res, $vec)")
    res
  }

  def dir(in: Long): (Long, Long) = {
    val s = math.sqrt(in).toLong
    val ring = s + (s % 2 - 1)
    val r2 = ring
    val rr = r2 * r2

    val dir =
      if (in < rr + ((ring + 1) * 1) - 1)
        (0L, 1L)
      else if (in < rr + ((ring + 1) * 2) - 1)
        (-1L, 0L)
      else if (in < rr + ((ring + 1) * 3) - 1)
        (0L, -1L)
      else
        (1L, 0L)
    dir
  }

  val positions = posVec(10)

  def idx(xy: (Long, Long)) = {
    val (x, y) = xy
    val m = max(abs(x), abs(y))

    val res =
      if ((x == -m) || (y == -m)) {
        val r = m + (m % 2)
        val r2 = r + 2
        val rr = r2 * r2

        val dx = x - -m
        val dy = m - y
        val d = dx + dy
        val res0 = rr + d + 1
        (m, rr, dx, dy, d, res0)
      } else {
        val r = m + (m % 2)
        val r2 = r + 1
        val rr = r2 * r2 - 1

        val dx = m - x
        val dy = y - -m
        val d = dx + dy
        val res0 = rr + d + 1

        (m, rr, dx, dy, d, res0)
      }

    if (m != 1 && m != 0)
      res._6 - 1
    else
      positions.indexOf(xy).toLong
  }

  def posVec(n: Long): Vector[(Long, Long)] =
    n match {
      case 0 => Vector((0, 0))
      case _ =>
        val vec = posVec(n - 1)
        val p1 = vec.last
        val p = p1 + dir(n - 1)
        vec :+ p
    }

  implicit class Tup2Add(val self: (Long, Long)) extends AnyVal {
    def +(other: (Long, Long)): (Long, Long) = {
      (self._1 + other._1, self._2 + other._2)
    }
  }
}
