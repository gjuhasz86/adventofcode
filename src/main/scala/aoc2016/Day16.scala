package aoc2016

object Day16 extends App {

  val input = "10111011111001111"

  def nextDragon(a: String): String = {
    println(s"${a.length}")
    val b = a.reverse.map {
      case '1' => '0'
      case '0' => '1'
    }
    s"${a}0${b}"
  }


  def dragon0(str: String, size: Int): String = {
    if (str.length > size)
      str
    else
      dragon0(nextDragon(str), size)
  }

  def dragon(str: String, size: Int) = dragon0(str, size).take(size)

  def chkSum(str: String): String = {
    println(s"CHK: $str")
    if (str.length % 2 == 0) {
      val newStr = str.grouped(2).map {
        case "00" | "11" => "1"
        case "01" | "10" => "0"
      }.mkString
      chkSum(newStr)
    } else {
      str
    }
  }
  println(input)
  val data = dragon(input, 35651584)
  println(s"*DATA: $data")
  val chk = chkSum(data)
  println(s"*CHK : $chk")

}
