package aoc2016

import common.ASearch
import common.ASearch.State

import scala.collection.immutable.{Set, SortedSet, TreeSet}
import scala.language.implicitConversions

object Day11 extends App {
  trait Item {
    def comp: String
  }
  case class Mch(comp: String) extends Item {
    override def toString: String = s"${comp}M"
  }
  case class Rtg(comp: String) extends Item {
    override def toString: String = s"${comp}G"
  }

  implicit def strToItem(str: String): Item = str.last match {
    case 'M' => Mch(str.dropRight(1))
    case 'G' => Rtg(str.dropRight(1))
  }

  implicit val itemPairOrd: Ordering[ItemPair] = Ordering.by(_.comp)
  case class ItemPair(comp: String, rtg: Int, mch: Int) {
    val isPaired: Boolean = rtg == mch
    val pair: (Int, Int) = (rtg, mch)

    def str(f: Int): String = rtgStr(f) + mchStr(f)
    def rtgStr(f: Int): String = if (f == rtg) s"${comp}G " else ".  "
    def mchStr(f: Int): String = if (f == mch) s"${comp}M " else ".  "

    def itemSet(f: Int): Set[Item] = rtgs(f) ++ mchs(f)
    def rtgs(f: Int): Set[Item] = if (f == rtg) Set(Rtg(comp)) else Set.empty
    def mchs(f: Int): Set[Item] = if (f == mch) Set(Mch(comp)) else Set.empty
  }

  def selectAtMost2[T](set: Set[T]): List[Set[T]] =
    set.toList.combinations(2).toList.map(_.toSet) ++ set.toList.map(e => Set(e))

  case class ChamberMove(dir: Int, items: Set[Item])

  case class ChamberState(curr: Int, top: Int, items: TreeSet[ItemPair]) extends State {
    override type OutState = ChamberState
    override type Move = ChamberMove

    val floors: Range.Inclusive = 1 to top

    override def moves: List[ChamberMove] = for {
      items <- selectAtMost2(items.flatMap(_.itemSet(curr)))
      dir <- List(1, -1).filter(d => floors.contains(curr + d))
    } yield ChamberMove(dir, items)

    override def next(m: ChamberMove): ChamberState =
      ChamberState(curr + m.dir, top, items.move(m.dir, m.items.toList))

    override def valid: Boolean = floors.contains(curr) && isSafe
    override def goal: Boolean = items.forall(_.pair == (top, top))
    override def estToGoal: Int = {
      val r1 = items.toList.flatMap(i => List(i.rtg, i.mch)).sorted
      val r2 = r1.map(h => top - h)
        .grouped(2)
        .toList
      val r3 = r2.map(_.max)
      val extra = Math.abs((top - curr) - r3.head)
      val r4 = extra :: r3.head :: r3.tail.map(_ * 2)
      r4.sum
    }

    def isSafe: Boolean = items.forall { item =>
      item.isPaired || !items.exists(i => i.rtg == item.mch)
    }

    val normalizedItems: List[(Int, Int)] = items.toList.map(_.pair).sorted

    override def equals(o: scala.Any): Boolean = o match {
      case that: ChamberState => curr == that.curr && top == that.top && normalizedItems == that.normalizedItems
      case _ => false
    }

    override def hashCode(): Int = 41 * (41 * (41 + curr) + top) + normalizedItems.hashCode

    override def toString: String = (1 to top).reverse.map { f =>
      val itStr = items.toList.map(_.str(f)).mkString
      val cStr = if (f == curr) "*" else " "
      s"$f$cStr | $itStr"
    }.mkString("\n")
  }

  implicit class RichTreeSet(items: TreeSet[ItemPair]) {
    def toState(curr: Int, top: Int) = ChamberState(curr, top, items)

    def move(dir: Int, its: List[Item]): TreeSet[ItemPair] = its match {
      case Nil => items
      case h :: t => move(dir, h).move(dir, t)
    }

    def move(dir: Int, item: Item): TreeSet[ItemPair] = {
      val oldPair = items.find(_.comp == item.comp).get
      val ItemPair(comp, rtg, mch) = oldPair
      val newPair = item match {
        case _: Rtg => ItemPair(comp, rtg + dir, mch + 0)
        case _: Mch => ItemPair(comp, rtg + 0, mch + dir)
      }
      val r = (items - oldPair) + newPair
      r
    }
  }


  val itemsSample = TreeSet(
    ItemPair("H", 2, 1),
    ItemPair("L", 3, 1)
  )
  val itemsPart1 = TreeSet(
    ItemPair("S", 1, 1),
    ItemPair("P", 1, 1),
    ItemPair("R", 2, 2),
    ItemPair("C", 2, 2),
    ItemPair("T", 2, 3)
  )

  val itemsPart2 = itemsPart1 ++ TreeSet(
    ItemPair("E", 1, 1),
    ItemPair("D", 1, 1)
  )

  def part1(): Int = {
    val res = ASearch.search(itemsPart1.toState(1, 4)).get
    res.printPath()
    println()
    println(res.pathLenght)
    res.pathLenght
  }
  def part2(): Int = {
    val res = ASearch.search(itemsPart2.toState(1, 4)).get
    res.printPath()
    println()
    println(res.pathLenght)
    res.pathLenght
  }

  val resPart1 = part1()
  val resPart2 = part2()

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")

}
