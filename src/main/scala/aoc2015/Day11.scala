package aoc2015

object Day11 extends App {

  val chars = "abcdefghjkmnpqrstuvwxyz"
  println(chars.length)

  def doubles(str: String) = str.sliding(2).filter(x => x(0) == x(1)).toList.distinct
  def hasTwoDoubles(str: String) = doubles(str).size > 1
  def hasAbc(str: String) = str.sliding(3).toList.exists(str => str(0) + 1 == str(1) && str(0) + 2 == str(2))

  def inc(str: String): String = str.toList match {
    case Nil => ""
    case heads :+ 'z' => inc(heads.mkString) + "a"
    case heads :+ c => (heads :+ chars(chars.indexOf(c) + 1)).mkString
  }

  def find(str:String):String={
    if(hasTwoDoubles(str) && hasAbc(str)) str else find(inc(str))
  }

  val start = "hepxcrrq"
  val res = find(start)
  println(res)
  val res2 = find(inc(res))
  println(res2)
}
