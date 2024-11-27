package aoc2016

object Day15 extends App {
  case class Disc(idx: Int, size: Int, init: Int) {
    val variant = size - ((size - init) % size)

    def atTime(t: Int) = (t + init) % size
    val firstGap = size - (init % size)
    val firstDrop = size - ((init + idx) % size)
    def nthDrop(n: Int) = n * size + firstDrop
    def drops = Stream.from(0).map(nthDrop)
    def sample = drops.take(20).toList
    def normalized = Disc(0, size, init + idx)

    def p1: String = s"$this [$variant] [$firstGap|$firstDrop] | $sample"
    def pp1: Unit = println(p1)
    def p2: String = s"$this [$variant] [$firstGap|$firstDrop] | ${(0 to 20).map(atTime).mkString("\t")}"
    def pp2: Unit = println(p2)

  }

  def combine(disc1: Disc, disc2: Disc): Disc = {
    val (dlRaw, dsRaw) = if (disc1.size > disc2.size) (disc1, disc2) else (disc2, disc1)

    println("----- LOOP -----")
    val dl = dlRaw.normalized
    val ds = dsRaw.normalized

    dlRaw.pp1
    dl.pp1
    println()
    dsRaw.pp1
    ds.pp1
    println()
    dlRaw.pp2
    dsRaw.pp2
    println()

    dl.pp1

    (0 until ds.size).foreach { i =>
      val d = Disc(ds.idx, ds.size, i)
      if (ds.variant % ds.size == i)
        println("* " + d.p1)
      else
        println("  " + d.p1)
    }

    val firstVariant = ds.size - (dl.firstDrop % ds.size)
    val step = ds.size - (dl.size % ds.size)
    println(s"$firstVariant by $step mod ${ds.size}")
    val variants = (0 until ds.size).map(i => (i * step + firstVariant) % ds.size)
    val variantIdx = variants.indexOf(ds.variant % ds.size)
    println(variants)
    println(variantIdx)
    println(dl.nthDrop(variantIdx))

    val newSize = ds.size * dl.size
    val newFirstDrop = dl.nthDrop(variantIdx)
    val newInit = newSize - newFirstDrop

    val newDisc = Disc(0, newSize, newInit)
    newDisc.pp1
    newDisc
  }

  println()

  val discsPart1 = List(
    Disc(1, 13, 1),
    Disc(2, 19, 10),
    Disc(3, 3, 2),
    Disc(4, 7, 1),
    Disc(5, 5, 3),
    Disc(6, 17, 5)
    //    Disc(7, 11, 0)
  )
  val discsPart2 = discsPart1 :+ Disc(7, 11, 0)


  val part1Res = discsPart1.tail.foldLeft(discsPart1.head)(combine)
  val part2Res = discsPart2.tail.foldLeft(discsPart2.head)(combine)

  println()
  println(s"PART1: ${part1Res.firstDrop}")
  println(s"PART2: ${part2Res.firstDrop}")
}
