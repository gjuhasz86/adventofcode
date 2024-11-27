package aoc2015

import scala.collection.mutable

object Day10 extends App {

  val input = "3113322113"
  val SameRegex = """((.)\2*)(.*?)""".r
  def next(str: String, acc: String = ""): String = str match {
    case "" => acc
    case SameRegex(as, a, rest) => next(rest, acc + as.length.toString + a)
  }

  def split3s(str: String): IndexedSeq[String] = str.split("(?<=3)(?!3)")

  val cache = mutable.Map[String, String]()
  val nextAndCache: String => String = (str: String) => {
    val s = next(str)
    cache += (str -> s)
    s
  }

  val res = (0 to 100).foldLeft(input) { (acc, i) =>
    println(s"$i | ${acc.length} | ${cache.size}")
    val s1 = split3s(acc)
//    println(s1)
    val s2 = s1.map(s => (s, cache.lift(s)))
//    println(s2)
    val s3 = s2.map(x => x._2.getOrElse(nextAndCache(x._1)))
//    println(s3)
//    println(s3.mkString)
//    println(next(acc))
    s3.mkString
  }
}
