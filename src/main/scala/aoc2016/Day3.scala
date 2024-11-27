package aoc2016

object Day3 extends App {
  // 10:14:20
  // 10:30:44 ~ 00:16:24
  // 10:38:44 ~ 00:08:00
  //          ~ 00:24:24

  val testInput =
    """ 101 301 501
      | 102 302 502
      | 103 303 503
      | 201 401 601
      | 202 402 602
      | 203 403 603""".stripMargin

  def parseLine(str: String) = {
    val parts = str.split(" +")
    (parts(1).toInt, parts(2).toInt, parts(3).toInt)
  }
  val parsed = Day3Input.input.lines.toList.map(parseLine)
  println(parsed)

  val isValid = (a: Int, b: Int, c: Int) => a + b > c && a + c > b && b + c > a

  def getAnswer(in: List[(Int, Int, Int)]) = {
    val valids = in.map(x => isValid.tupled(x))
    println(valids)
    val validCount = valids.filter(x => x).size
    println(validCount)
  }
  getAnswer(parsed)

  val regrouped = parsed.grouped(3).toList.flatMap {
    case List((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) => List((a1, b1, c1), (a2, b2, c2), (a3, b3, c3))
    case _ => throw new IllegalArgumentException("nope")
  }
  println(regrouped)

  getAnswer(regrouped)
}

