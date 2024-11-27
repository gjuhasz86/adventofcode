package aoc2016

import scala.io.Source

object Day10 extends App {

  val input = Source.fromFile("src/main/scala/day10.txt").getLines().toList
  println(input)

  //  val input = List(
  //    "value 5 goes to bot 2",
  //    "bot 2 gives low to bot 1 and high to bot 0",
  //    "value 3 goes to bot 1",
  //    "bot 1 gives low to output 1 and high to bot 0",
  //    "bot 0 gives low to output 2 and high to output 0",
  //    "value 2 goes to bot 2"
  //  )

  trait Container {
    def id: String
    def accept(chip: Int): Container
    def action(containers: List[Container]): List[Container]
  }

  case class InBin(chipOpt: Option[Int], target: String) extends Container {
    override def id = ""
    override def accept(chip: Int): Container = this

    override def action(containers: List[Container]): List[Container] =
      chipOpt.map(ch => action(containers, ch)).getOrElse(containers)

    private def action(containers: List[Container], chip: Int): List[Container] = containers.collect {
      case c if c.id == target => c.accept(chip)
      case c if c eq this => InBin(None, target)
      case c => c
    }
  }
  case class OutBin(id: String, chips: List[Int]) extends Container {
    override def accept(chip: Int) = OutBin(id, chip :: chips)
    override def action(containers: List[Container]): List[Container] = containers
  }
  case class Bot(id: String, chips: List[Int], lowTarget: String, highTarget: String) extends Container {
    override def accept(chip: Int) = Bot(id, chip :: chips, lowTarget, highTarget)
    def action(containers: List[Container]): List[Container] =
      if (chips.size == 2)
        action(containers, chips.min, chips.max)
      else
        containers

    private def action(containers: List[Container], lowChip: Int, highChip: Int): List[Container] = containers.collect {
      case cont if cont.id == lowTarget => cont.accept(lowChip)
      case cont if cont.id == highTarget => cont.accept(highChip)
      case cont if cont.id == id => Bot(id, Nil, lowTarget, highTarget)
      case cont => cont
    }
  }

  val InRegex = """value (.+) goes to (bot .+)""".r
  val BotRegex = """(bot .+) gives low to (.+) and high to (.+)""".r
  def parse(str: String): Container = str match {
    case InRegex(chip, target) => InBin(Some(chip.toInt), target)
    case BotRegex(id, lowTgt, highTgt) => Bot(id, Nil, lowTgt, highTgt)
  }

  val parsed = input map parse
  parsed foreach println

  def addOutBins(containers: List[Container]): List[Container] = {
    containers ::: outputs(containers).filterNot(id => containers.map(_.id).contains(id)).map(id => OutBin(id, Nil))
  }
  def outputs(containers: List[Container]): List[String] = containers.flatMap {
    case Bot(_, _, out1, out2) => out1 :: out2 :: Nil
    case InBin(_, out) => out :: Nil
  }

  println()
  val system = addOutBins(parsed)
  system foreach println

  def simulate(containers: List[Container], goal: (Int, Int), goalFound: Option[String]): (Option[String], List[Container]) = {
    println("--- STEP ---")
    containers.map { case b@Bot(_, chips, _, _) if chips.contains(goal._1) && chips.contains(goal._2) => s"* $b"; case x => x } foreach println
    val goalBot =
      containers.collect { case b@Bot(_, chips, _, _) if chips.contains(goal._1) && chips.contains(goal._2) => b.id }
    val newConts = containers.foldLeft(containers)((acc, c) => c.action(acc))
    if (newConts == containers)
      (goalFound, newConts)
    else
      simulate(newConts, goal, goalFound.orElse(goalBot.headOption))
  }

  println()
  val (goalBotOpt, endState) = simulate(system, (61, 17), None)
  endState foreach println
  println(goalBotOpt)
  val resPart1 = goalBotOpt.get.drop(4)

  val outChips = endState.collect { case OutBin(id, chips) if id == "output 0" || id == "output 1" || id == "output 2" => chips }.flatten
  val resPart2 = outChips.product
  println(resPart2)

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")


}
