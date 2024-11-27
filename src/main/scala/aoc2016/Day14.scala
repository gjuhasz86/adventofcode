package aoc2016

import java.security.MessageDigest

import scala.util.matching.Regex

object Day14 extends App {
  //  val salt = "abc"
  val salt = "yjdafjpo"
  val Mult3Regex: Regex = """(.)(?=\1\1)""".r
  val Mult5Regex: Regex = """(.)(?=\1\1\1\1)""".r

  def md5Orig(s: String): String = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def md5Stretch(s: String, n: Int = 2016): String = {
    if (n > 0)
      md5Stretch(md5Orig(s), n - 1)
    else
      md5Orig(s)
  }

  var md5 = md5Orig _


  def check(r: Regex)(i: Int): Option[(Int, String, Char)] = {
    val hash = md5(salt + i.toString)
    r.findFirstMatchIn(hash).map(_.toString.head).map(c => (i, hash, c))
  }

  def find(range: Range): (Seq[(Int, String, Char)], Seq[(Int, String, Char)]) = {
    val res3 = range.par.flatMap { i =>
      check(Mult3Regex)(i)
    }.seq
    val res5 = range.par.flatMap { i =>
      check(Mult5Regex)(i)
    }.seq
    (res3, res5)
  }


  def generate(cache3: List[(Int, String, Char)], cache5: List[(Int, String, Char)]) = {
    val allHashes = cache3 zip cache3.map { case (i3, h3, c3) =>
      cache5.exists { case (i5, h5, c5) => i3 < i5 && i5 <= i3 + 1000 && c3 == c5 }
    }
    val goodHashes = allHashes.filter(_._2).map(_._1)
    goodHashes
  }

  val batchSize = 100
  def findAll(n: Int, cache3acc: List[(Int, String, Char)] = Nil, cache5acc: List[(Int, String, Char)] = Nil): Seq[(Int, String, Char)] = {
    val (cache3, cache5) = find(n * batchSize until (n + 1) * batchSize)


    val newCache3Acc = cache3.toList ::: cache3acc
    val newCache5Acc = cache5.toList ::: cache5acc

    val goodHashes = generate(newCache3Acc, newCache5Acc)
    println(s"${n * batchSize} | ${goodHashes.size} | $goodHashes")
    if (goodHashes.size < 70)
      findAll(n + 1, newCache3Acc, newCache5Acc)
    else
      goodHashes
  }

  def part1(): Int = {
    md5 = md5Orig _
    val res = findAll(0)
    res.sortBy(_._1).zipWithIndex foreach println
    res(63)._1
  }

  def part2(): Int = {
    md5 = md5Stretch(_)
    val res = findAll(0)
    res.sortBy(_._1).zipWithIndex foreach println
    res(63)._1
  }

  val resPart1 = part1()
  val resPart2 = part2()

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")

}
