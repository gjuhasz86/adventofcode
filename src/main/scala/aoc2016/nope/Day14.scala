package aoc2016.nope

import java.security.MessageDigest

import scala.collection.mutable

object Day14 extends App {

  val salt = "abc"
  val Mult3Regex = """(.)(?=\1\1)""".r
  val Mult5Regex = """(.)(?=\1\1\1\1)""".r

  val hashcache = mutable.Map[String, String]()
  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  def md5stretch(s: String, n: Int = 2016): String = {
    if (n > 0)
      md5stretch(md5(s), n - 1)
    else
      md5(s)
  }


  val found3 = mutable.Map[Char, List[(Int, String)]]().withDefaultValue(Nil)

  //  val cache3 = mutable.ListBuffer[(Int, String, List[Char])]()
  //  val cache5 = mutable.ListBuffer[(Int, String, List[Char])]()


  def find(i: Int, until: Option[Int], acc: List[(Int, String)] = Nil): List[(Int, String)] = {
    if (until.isDefined && i >= until.get) {
      println(s"Done [$i]")
      acc
    } else {
      val hash = md5stretch(salt + i.toString)
      if (i % 100 == 0) {
        println(s"$i | ${acc.size} | $hash")
      }
      val code3s = Mult3Regex.findFirstMatchIn(hash).map(_.toString.head).toList.distinct
      val code5s = Mult5Regex.findFirstMatchIn(hash).map(_.toString.head).toList.distinct
      //      val code3s = Mult3Regex.findAllMatchIn(hash).map(_.toString.head).toList.distinct
      //      val code5s = Mult5Regex.findAllMatchIn(hash).map(_.toString.head).toList.distinct

      //      if (code3s.nonEmpty) {
      //        cache3 += ((i, hash, code3s))
      //      }
      //      if (code5s.nonEmpty) {
      //        cache5 += ((i, hash, code5s))
      //      }

      val goods = code5s.flatMap { c =>
        val lastSeen = found3(c).filter(idx => (i - idx._1) <= 1000)
        println(s" $i | ${lastSeen.mkString} | $hash")
        lastSeen
      }
      code3s.foreach { c =>
        val old = found3(c)
        found3 += (c -> ((i, hash) :: old))
      }

      val newAcc = goods ::: acc
      if (newAcc.size < 70) {
        find(i + 1, until, newAcc)
      } else if (until.isDefined) {
        println(s"* Passing $i [$until]")
        find(i + 1, until, newAcc)
      } else {
        println(s"* Finishing $i [$until]")
        find(i + 1, Some(i + 1002), newAcc)
      }
    }
  }

  println
  val res = find(1, None)
  res.sorted.zipWithIndex foreach println
  //  println(found3)

  //  val res2 = cache3 zip cache3.map { case (i, h, cs) =>
  //    cache5.filter(_._1 > i).filter(_._1 <= i + 1000).filter(cs5 => cs5._3.intersect(cs).nonEmpty)
  //  }

  //  println("---- ----")
  //  val final2 = res2.filter(_._2.nonEmpty).map(_._1).zipWithIndex
  //  final2 foreach println

}
