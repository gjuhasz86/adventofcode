package aoc2015

object Day14 extends App {
  val deers1 = List(
    Deer("Comet", 14, 10, 127),
    Deer("Dancer", 16, 11, 162)
  )
  val deers = List(
    Deer("Dancer", 27, 5, 132),
    Deer("Cupid", 22, 2, 41),
    Deer("Rudolph", 11, 5, 48),
    Deer("Donner", 28, 5, 134),
    Deer("Dasher", 4, 16, 55),
    Deer("Blitzen", 14, 3, 38),
    Deer("Prancer", 3, 21, 40),
    Deer("Comet", 18, 6, 103),
    Deer("Vixen", 18, 5, 84)
  )

  case class Deer(name: String, speed: Int, stamina: Int, rest: Int)

  val raceLength = 2503

  val lengths = deers.map { d =>
    val rounds = raceLength / (d.stamina + d.rest)
    val remainder = raceLength % (d.stamina + d.rest)
    val fullRunLength = rounds * d.stamina * d.speed
    val partRunLength = Math.min(remainder, d.stamina) * d.speed
    fullRunLength + partRunLength
  }

  val res = deers.map(_.name).zip(lengths)
  res.sortBy(_._2) foreach println

  case class Position(deer: Deer, phase: Int, travelled: Int, points: Int)

  def simulateOne(positions: List[Position]): List[Position] = {
    val nextPositions = positions.map { pos =>
      if (pos.phase < pos.deer.stamina)
        pos.copy(travelled = pos.deer.speed + pos.travelled, phase = pos.phase + 1)
      else if (pos.phase < pos.deer.stamina + pos.deer.rest)
        pos.copy(phase = pos.phase + 1)
      else
        pos.copy(travelled = pos.deer.speed + pos.travelled, phase = 1)
    }

    val leadPos = nextPositions.map(_.travelled).max
    nextPositions.map { pos =>
      if (pos.travelled == leadPos)
        pos.copy(points = pos.points + 1)
      else
        pos
    }
  }

  def simulate(positions: List[Position], sec: Int): List[Position] = {
    println(sec)
    positions foreach println
    if (sec == raceLength)
      positions
    else
      simulate(simulateOne(positions), sec + 1)
  }

  val result = simulate(deers.map(d => Position(d, 0, 0, 0)), 0)

  println()
  result.sortBy(_.points).reverse foreach println
}
