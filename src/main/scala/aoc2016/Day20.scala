package aoc2016

import scala.io.Source

object Day20 extends App {

  val input = Source.fromFile("src/main/resources/aoc2016/day20.txt").getLines().toList
  val input0 = List(
    "5-8",
    "0-2",
    "4-7"
  )

  val RangePat = """([0-9]+)-([0-9]+)""".r
  def parse(str: String): (Long, Long) =
    str match {
      case RangePat(lo, hi) => (lo.toLong, hi.toLong)
    }

  val ranges = (input map parse).sortBy(_._1)
  ranges foreach println

  def findLowest(rs: List[(Long, Long)], min: Long): Long = rs match {
    case Nil => min
    case (lo, hi) :: tail if lo > min => println(s"maybe: $min"); findLowest(tail, min)
    case (lo, hi) :: tail if lo <= min && min <= hi => findLowest(tail, hi + 1)
    case (lo, hi) :: tail => findLowest(tail, min)
  }

  //  val res1 = findLowest(ranges, 0)
  //  println(res1)

  def diff(r1: (Long, Long), r2: (Long, Long)): List[(Long, Long)] = (r1, r2) match {
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l2, l1, h1, h2) => List()
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l1, l2, h1, h2) => List((l1, l2 - 1))
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l2, l1, h2, h1) => List((h2 + 1, h1))
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l1, l2, h2, h1) => List((l1, l2 - 1), (h2 + 1, h1))
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l2, h2, l1, h1) => List((l1, h1))
    case ((l1, h1), (l2, h2)) if List(l1, h1, l2, h2).sorted == List(l1, h1, l2, h2) => List((l1, h1))
  }

  assert(diff((3, 10), (3, 12)) == Nil)
  assert(diff((3, 10), (4, 12)) == List((3, 3)))
  assert(diff((3, 10), (2, 10)) == Nil)
  assert(diff((3, 10), (3, 10)) == Nil)
  assert(diff((3, 10), (2, 11)) == Nil)
  assert(diff((3, 10), (2, 9)) == List((10, 10)))
  assert(diff((3, 10), (2, 8)) == List((9, 10)))
  assert(diff((3, 10), (4, 8)) == List((3, 3), (9, 10)))
  assert(diff((3, 10), (5, 8)) == List((3, 4), (9, 10)))
  assert(diff((3, 10), (1, 2)) == List((3, 10)))
  assert(diff((3, 10), (1, 3)) == List((4, 10)))
  assert(diff((3, 10), (10, 30)) == List((3, 9)))
  assert(diff((3, 10), (11, 30)) == List((3, 10)))

  def findAll(rs: List[(Long, Long)], acc: List[(Long, Long)]): List[(Long, Long)] = rs match {
    case Nil => acc
    case r :: tail =>
      val newAcc = acc.flatMap(r0 => diff(r0, r))
      findAll(tail, newAcc)
  }

  val resRanges = findAll(ranges, List((0, 4294967295L)))
  println(resRanges)
  val elements= resRanges.map { case (lo, hi) => hi - lo + 1 }
  println(elements)
  println(elements.sum)
}
