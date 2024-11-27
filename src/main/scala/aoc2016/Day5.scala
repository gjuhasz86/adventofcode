package aoc2016

import java.security.MessageDigest

object Day5 extends App {

  val input = "reyedfim"

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def find(i: Int, until: Int): List[(Int, String)] = {
    if (i >= until) {
      Nil
    } else {
      val hash = md5(input + i.toString)
      //      if (i % 100000 == 0) {
      //        println((i, hash))
      //      }
      if (hash.startsWith("00000"))
        (i, hash) :: find(i + 1, until)
      else
        find(i + 1, until)
    }
  }


  val batch = 100000

  var n = 0
  var found = List[(Int, String)]()

  def genCode(list: List[(Int, String)]): Option[String] = {
    val codeparts = list.map { case (n, s) => (s.charAt(5), s.charAt(6)) }

    val relevant = for (i <- 0 to 7) yield {
      codeparts.find(_._1 == s"$i".head)
    }

    println(s"Found so far $relevant")
    val code = relevant.flatten.map(x => x._2).mkString
    println(s"Code so far: [$code]")
    println(s"Size: ${code.size}")

    if (code.size < 8)
      None
    else
      Some(code)
  }

  while (genCode(found).isEmpty) {
    val res = (0 to 7).par.map { i =>
      println(found)
      println(s"Starting batch #$n.$i (${((n * 8) + i) * batch} - ${((n * 8) + i + 1) * batch})")
      val r = find(((n * 8) + i) * batch, ((n * 8) + i + 1) * batch)
      println(s"Finished batch: #$n.$i: $r")
      r
    }.toList.flatten
    found = found ::: res
    n = n + 1
  }
  println()
  found foreach println
  println()
  val code = found.map { case (n, s) => s.charAt(5) }.mkString
  println(code)
}

//    .aggregate(List[(Int, String)]())((a, b) => {
//    println(s"seqop: $a $b")
//    a ::: b
//  }, (a, b) => {
//    println(s"combop: $a $b")
//    a ::: b
//  })

