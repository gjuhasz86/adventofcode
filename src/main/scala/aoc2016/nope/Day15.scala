package aoc2016.nope

object Day15 extends App {
  case class Disc(pos: Int, slots: Int, start: Int) {
    val normStart = pos + start
    val times = Stream.from(0).map(_ * slots + normStart)
    def sample = this.times.take(20).toList
    def aligned(nth: Int) = nth * slots + normStart
    def normalized = Disc(0, slots, start + pos)
    def atTime(t: Int) = (t + start) % slots
  }

  val sculpture = List(
    Disc(1, 13, 1),
    Disc(2, 19, 10),
    Disc(3, 3, 2),
    Disc(4, 7, 1),
    Disc(5, 5, 3),
    Disc(6, 17, 5)
  )


  //  val sculptureTest = List(
  //    Disc(2, 19, 1),
  //    Disc(1, 13, 1),
  //    Disc(1, 13, 2),
  //    Disc(1, 13, 3),
  //    Disc(1, 13, 4),
  //    Disc(1, 13, 5),
  //    Disc(1, 13, 6),
  //    Disc(1, 13, 7),
  //    Disc(1, 13, 8),
  //    Disc(1, 13, 9),
  //    Disc(1, 13, 10),
  //    Disc(1, 13, 11),
  //    Disc(1, 13, 12)
  //  )
  //
  //  sculptureTest.foreach {
  //    disc =>
  //      val x = disc.times.take(20).toList
  //      println(s"${
  //        disc.pos
  //      }| $disc  \t| $x")
  //  }

  def largeFirstTime(large: Disc, small: Disc): Stream[Int] = {
    val x = large.times.filter(_ >= small.times.head)
    x
  }

  println()
  def combine(d1: Disc, d2: Disc) = {
    val large = List(d1, d2).maxBy(_.slots)
    val small = List(d1, d2).minBy(_.slots)

    println()
    println(s"${
      large.pos
    }| $large \t| ${
      large.times.take(20).toList
    }")
    println(s"${
      small.pos
    }| $small \t| ${
      small.times.take(20).toList
    }")

    val largeStream = largeFirstTime(large, small)
    val largeFirst = largeStream.head
    val smallCompatPos = (largeFirst % small.slots + small.pos) % small.slots
    val x = smallCompatPos - small.start
    val sameOffset = if (x < 0) x + small.slots else x
    val firstMatch = largeStream.drop(sameOffset).head
    val nextMatch = firstMatch + small.slots * large.slots

    println(s"$largeFirst $smallCompatPos $sameOffset | $firstMatch $nextMatch")
    val combDisc = Disc(0, small.slots * large.slots, firstMatch)
    println(combDisc)
    println(combDisc.times.take(20).toList)
    combDisc
  }

  //  compat(Disc(1, 6, 0), Disc(2, 4, 3)) // doesnt work on non-primes
  //  combine(Disc(1, 7, 1), Disc(2, 3, 1))


  sculpture.foreach { d =>
    println(s"$d \t| ${d.sample}")
    println(s"${d.normalized} \t| ${d.normalized.sample}")
  }

  def calc(dl: Disc, ds: Disc) = {
    println()
    println(s"$dl | ${dl.sample}")
    println(s"$ds | ${ds.sample}")
    println()
    //    (0 until ds.slots).foreach { i =>
    //      val d = Disc(ds.pos, ds.slots, i)
    //      if (ds.start == i)
    //        println(s"* $d | ${d.sample}")
    //      else
    //        println(s"  $d | ${d.sample}")
    //    }

    // real logic starts here
    val first = dl.start % ds.slots
    val step = dl.slots % ds.slots
    println(s"$first by $step mod ${ds.slots}")
    val s = (0 until ds.slots).map(_ * step).map(_ + first).map(_ % ds.slots)
    val idx = s.indexOf(ds.start % ds.slots)
    println(s)
    println(idx)
    val newStart = dl.aligned(idx)
    val newSlots = dl.slots * ds.slots
    println(newStart)
    println(dl.aligned(idx + ds.slots))
    val newIdx2 = ((ds.start - 1 - newStart + newSlots) / newSlots)
    println(newIdx2)
    val newDisc0 = Disc(0, newSlots, newStart)
    println(newDisc0)
    println(newDisc0.sample)
    val newStart2 = newDisc0.aligned(newIdx2)
    val newDisc = Disc(0, newSlots, newStart2)
    println(newDisc)
    println(newDisc.sample)
    newDisc
  }

  println()
  //  calc(Disc(0, 5, 1), Disc(0, 3, 27))
  //  calc(Disc(1, 19, 1).normalized, Disc(2, 13, 10).normalized)
  //  val s2 = sculpture.map(_.normalized).sortBy(_.slots).reverse

  val s2 = List(
    //    Disc(0, 5, 5),
    //    Disc(0, 2, 3)
    Disc(0, 5, 4),
    Disc(1, 5, 3)
    //    Disc(0, 7, 5),
    //    Disc(0, 13, 2),
    //    Disc(0, 17, 11),
    //    Disc(0, 19, 12)
  )
  s2 foreach println
  val res = s2.tail.foldLeft(s2.head)(calc)

  //  val res = sculpture.tail.foldLeft(sculpture.head)(combine)
  //    println(res)

  println()
  s2.foreach { d =>
    println(d + " \t| \t" + (0 to 15).map(d.atTime).mkString("\t"))
    //    println(d.normalized + " \t| \t" + (0 to 30).map(d.normalized.atTime).mkString("\t"))
  }

  println()

  println(Disc(1, 5, 4).sample)

}
