package aoc2016.nope

import common.ASearch

import scala.collection.immutable.Set
import scala.language.implicitConversions

object Day11v1 extends App {


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

  case class Layout(layout: Map[Int, Set[Item]]) {

    def moveItems(from: Int, to: Int, items: Set[Item]): Layout =
      this.removeItems(from, items).addItems(to, items)

    private def removeItems(floor: Int, items: Set[Item]) = Layout {
      layout + (floor -> (layout(floor) -- items))
    }

    private def addItems(floor: Int, items: Set[Item]) = Layout {
      layout + (floor -> (layout(floor) ++ items))
    }
  }

  def selectAtMost2[T](set: Set[T]): List[Set[T]] =
    set.toList.combinations(2).toList.map(_.toSet) ++ set.toList.map(e => Set(e))

  case class ChamberMove(dir: Int, items: Set[Item])
  case class ChamberState(elevator: Int, layout: Layout, simple: Boolean) extends ASearch.State {
    override type OutState = ChamberState
    override type Move = ChamberMove

    def isSafe(items: List[Item]): Boolean = items.forall {
      case Mch(comp) => items.contains(Rtg(comp)) || items.collect { case Rtg(_) => () }.isEmpty
      case _ => true
    }

    override def valid: Boolean = elevator > 0 && elevator <= layout.layout.size &&
      (simple || layout.layout.values.toList.forall(its => isSafe(its.toList)))

    override def moves: List[ChamberMove] = for {
      items <- selectAtMost2(layout.layout(elevator))
      dir <- List(1, -1).filter(d => elevator + d > 0 && elevator + d <= layout.layout.size)
    } yield ChamberMove(dir, items)

    override def next(m: ChamberMove): ChamberState =
      ChamberState(elevator + m.dir, layout.moveItems(elevator, elevator + m.dir, m.items), simple)

    override def goal: Boolean = {
      val floors = layout.layout.toList.sortBy(_._1).reverse.map(_._2)
      floors.head.nonEmpty && floors.drop(1).forall(_.isEmpty)
    }

    override def estToGoal: Int =
      if (simple)
        calcHeur(elevator, layout.layout)
      else
        ASearch.search(this.copy(simple = true), ???).map(_.pathLenght).getOrElse(Int.MaxValue)

    //    override def toString: String =
    //      "[" + layout.layout.toList.map { case (f, its) =>
    //        val cf = if (elevator == f) "*" else ""
    //        s"$f$cf(${its.mkString(",")})"
    //      }.mkString(" | ") + "]"

    override def toString: String = {
      layout.layout.toList.map { case (f, items) =>
        val itemsStr = items.map(_.toString).mkString(", ")
        val elevStr = if (f == elevator) "*" else " "
        s"F$f |$elevStr $itemsStr"
      }.reverse.mkString("\n")
    }
  }


  val initialLayout = Layout {
    Map(
      1 -> Set("HM", "LM"),
      2 -> Set("HG"),
      3 -> Set("LG"),
      4 -> Set()
    )
  }


  //    val initialLayout = Layout {
  //      Map(
  //        1 -> Set("SG", "SM", "PG", "PM"),
  //        2 -> Set("TG", "RG", "RM", "CG", "CM"),
  //        3 -> Set("TM"),
  //        4 -> Set()
  //      )
  //    }

  def calcHeur(fl: Int, layout: Map[Int, Set[Item]]) = {
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
    val extra = Math.abs((top - fl) - r3.head)
    //    println(extra)
    val r4 = extra :: r3.head :: r3.tail.map(_ * 2)
    //    println(r4)
    r4.sum
  }

  val start: ChamberState = ChamberState(1, initialLayout, false)

  val resOpt = ASearch.search(start, ???)
  val endState = resOpt

  endState.foreach(_.printPath())

  println(endState.get.pathLenght)
  //  val resStr = resOpt.map { res =>
  //    res.mkString("\n\n") + s"\n ${res.size - 1}"
  //  } getOrElse {
  //    "No solution"
  //  }
  //  println(resStr)
}
