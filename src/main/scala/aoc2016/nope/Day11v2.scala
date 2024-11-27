package aoc2016.nope

import common.ASearch
import common.ASearch.State

import scala.collection.immutable.Set
import scala.language.implicitConversions

object Day11v2 extends App {
  trait Item
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

  type Layout = Map[Int, Set[Item]]
  implicit class RichLayout(val lo: Map[Int, Set[Item]]) extends AnyVal {

    def moveItems(from: Int, to: Int, items: Set[Item]): Layout =
      this.removeItems(from, items).addItems(to, items)

    def removeItems(floor: Int, items: Set[Item]): Map[Int, Set[Item]] =
      lo + (floor -> (lo(floor) -- items))

    def addItems(floor: Int, items: Set[Item]): Map[Int, Set[Item]] =
      lo + (floor -> (lo(floor) ++ items))
  }

  def selectAtMost2[T](set: Set[T], simple: Boolean): List[Set[T]] =
    set.toList.combinations(2).toList.map(_.toSet) ++ set.toList.map(e => Set(e)) ++
      (if (simple) List(Set[T]()) else Nil)

  case class ChamberMove(dir: Int, items: Set[Item])
  case class ChamberState(elevator: Int, layout: Layout, simple: Boolean) extends State {
    override type OutState = ChamberState
    override type Move = ChamberMove

    def isSafe(items: Set[Item]): Boolean = items.forall {
      case Mch(comp) => items.contains(Rtg(comp)) || items.collect { case Rtg(_) => () }.isEmpty
      case _ => true
    }

    override def valid: Boolean = layout.keySet.contains(elevator) &&
      layout.values.toList.forall(its => isSafe(its))

    override def moves: List[ChamberMove] = for {
      items <- selectAtMost2(layout(elevator), simple)
      dir <- List(1, -1).filter(d => layout.keySet.contains(elevator + d))
    } yield ChamberMove(dir, items)

    override def next(m: ChamberMove): ChamberState =
      ChamberState(elevator + m.dir, layout.moveItems(elevator, elevator + m.dir, m.items), simple)

    override def goal: Boolean = {
      val floors = layout.toList.sortBy(_._1).reverse.map(_._2)
      floors.head.nonEmpty && floors.drop(1).forall(_.isEmpty)
    }

    override def estToGoal: Int =
      estToGoalSimple
    //      if (simple)
    //        estToGoalSimple
    //      else
    //        ASearch.search(this.copy(simple = true), (_: Any, _: Any, _: Any) => "").map(_.pathLenght).getOrElse(Int.MaxValue / 2)

    def estToGoalSimple: Int = {
      val top = layout.size
      val r1 = layout.toList
        .flatMap { case (f, its) => its.toList.map(_ => f) }
      //    println(r1)
      val r2 = r1.map(h => top - h)
        .grouped(2)
        .toList
      //    println(r2)
      val r3 = r2.map(_.max)
      //    println(r3)
      val extra = Math.abs((top - elevator) - r3.head)
      //    println(extra)
      val r4 = extra :: r3.head :: r3.tail.map(_ * 2)
      //    println(r4)
      r4.sum
    }

    override def toString: String = {
      (if (simple) "simple\n" else "full\n") + layout.toList.map { case (f, items) =>
        val itemsStr = items.map(_.toString).mkString(", ")
        val elevStr = if (f == elevator) "*" else " "
        s"F$f |$elevStr $itemsStr"
      }.reverse.mkString("\n")
    }
  }

  //  val initialLayout: Layout = Map(
  //    1 -> Set("AG", "AM", "BG", "BM", "CG", "CM"),
  //    2 -> Set()
  //    3 -> Set()
  //    4 -> Set()
  //  )
  val initialLayout: Layout = Map(
    1 -> Set("SG", "SM", "PG", "PM"),
    2 -> Set("TG", "RG", "RM", "CG", "CM"),
    3 -> Set("TM"),
    4 -> Set()
  )

  val start: ChamberState = ChamberState(1, initialLayout, false)
  println("Starting")
  val resOpt = ASearch.search(start)
  resOpt.get.printPath()
  println()
  println(resOpt.get.pathLenght)
}
