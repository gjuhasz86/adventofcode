package aoc2015

import java.security.MessageDigest

object Day4 extends App {


  val input = "ckczppom"

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def find(i: Int): (Int, String) = {
    val hash = md5(input + i.toString)
    if (i % 100000 == 0) {
      println((i, hash))
    }
    if (hash.startsWith("000000"))
      (i, hash)
    else
      find(i + 1)
  }

  val r = find(0)
  println(r)

}
