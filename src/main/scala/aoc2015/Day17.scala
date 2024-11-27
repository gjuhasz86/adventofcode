package aoc2015

object Day17 extends App {

  case class Container(id: Int, volume: Int)

  val input =
    """50
      |44
      |11
      |49
      |42
      |46
      |18
      |32
      |26
      |40
      |21
      |7
      |18
      |43
      |10
      |47
      |36
      |24
      |22
      |40""".stripMargin

  val inputContainers0 =
    List(20, 15, 10, 5, 5)
      .zipWithIndex
      .map { case (v, i) => Container(i, v) }

  val inputContainers =
    input.lines.toList
      .map(_.toInt)
      .zipWithIndex
      .map { case (v, i) => Container(i, v) }

  type Comb = Map[Container, Int]
  var counter = 0
  def fit(containers: List[Container], amount: Int): List[Comb] = {
    def loop(soFar: List[Container]): List[Comb] = {
      val left = amount - soFar.map(_.volume).sum
      if (left == 0) {
        counter = counter + 1
        if (counter % 10000 == 0) println(counter)
        List(soFar.groupBy(identity).mapValues(_.size))
      } else if (left < 0) {
        counter = counter + 1
        if (counter % 10000 == 0) println(counter)
        List()
      } else {
        (containers diff soFar).flatMap(c => loop(c :: soFar))
      }
    }
    loop(Nil).distinct
  }
//  val res = fit(inputContainers0, 25)
    val res = fit(inputContainers, 150)
  res foreach println
  println(res.size)

  val minContainers = res.minBy(_.size).size
  val allMinContainers = res.filter(_.size == minContainers)
  println(allMinContainers.size)
}
