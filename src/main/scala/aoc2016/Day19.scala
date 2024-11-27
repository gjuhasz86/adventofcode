package aoc2016

object Day19 extends App {

  val elfCount = 3004953
  val a = Integer.highestOneBit(elfCount)
  val l = elfCount - a
  val winner = 2 * l + 1
  println(winner)


  def simOne(es: List[Int]): List[Int] = {
    val next = es.size / 2
    //    println(s"Out: ${es(next)}")
    val (h :: t) = es.take(next) ::: es.drop(next + 1)
    t :+ h
  }

  def sim(es: List[Int]): List[Int] = {
    //    println(es)
    //    if (es.size == Integer.highestOneBit(es.size)){
    //      println(s"Win: ${es.head}")
    //    }
    if (es.size == 1)
      es
    else
      sim(simOne(es))
  }

  def rem(np: Int): (Int, Int, Int) = {
    val n = np - 1
    val base3 = Integer.toString(n, 3)
    val highest = base3.head.toString.toInt
    val tail = if (base3.tail.isEmpty) "0" else base3.tail
    val rest = Integer.parseInt(tail, 3)
    val highestVal = (Math.pow(3, base3.tail.size)).toInt
    (highestVal, highest, rest)
  }


  //  (1 to 500).foreach { n =>
  //    val elves = (1 to n).toList
  //    val win = sim(elves).head
  //    val (hv, h, r) = rem(n)
  //    val x = (n - (hv * h)) * h
  //    val y = hv * (h - 1)
  //    println(s"$n\t$win\t[${hv * h}, $hv, $h, $r] = ${x + y}")
  //    println(s"$n\t$win\t = ${acrossWin(n)}")
  //
  //  }

  def acrossWin(n: Int): Int = {
    val (hv, h, r) = rem(n)
    val x = (n - (hv * h)) * h
    val y = hv * (h - 1)
    x + y
  }
  println(acrossWin(elfCount))
}
