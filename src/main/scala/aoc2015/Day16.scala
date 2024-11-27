package aoc2015

import scala.io.Source

/*
--- Day 16: Aunt Sue ---

Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card. However, there's a small problem: she signed it "From, Aunt Sue".

You have 500 Aunts named "Sue".

So, to avoid sending the card to the wrong person, you need to figure out which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the gift. You open the present and, as luck would have it, good ol' Aunt Sue got you a My First Crime Scene Analysis Machine! Just what you wanted. Or needed, as the case may be.

The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few specific compounds in a given sample, as well as how many distinct kinds of those compounds there are. According to the instructions, these are what the MFCSAM can detect:

children, by human DNA age analysis.
cats. It doesn't differentiate individual breeds.
Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
goldfish. No other kinds of fish.
trees, all in one group.
cars, presumably by exhaust or gasoline or something.
perfumes, which is handy, since many of your Aunts Sue wear a few kinds.
In fact, many of your Aunts Sue have many of these. You put the wrapping from the gift into the MFCSAM. It beeps inquisitively at you a few times and then prints out a message on ticker tape:

children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
You make a list of the things you can remember about each Aunt Sue. Things missing from your list aren't zero - you simply don't remember the value.

What is the number of the Sue that got you the gift?
 */
object Day16 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  case class Sue(id: Int, children: Int, cats: Int, samoyeds: Int, pomeranians: Int,
                 akitas: Int, vizslas: Int, goldfish: Int, trees: Int, cars: Int, perfumes: Int)

  val GenericSue = Sue(0, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue,
    Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue)

  def parse(str: String): Sue = str match {
    case r"Sue (.+?)$id: (.+)$props" =>
      val sue = GenericSue.copy(id = id.toInt)
      props.split(", ").foldLeft(sue) {
        case (s, r"children: (.+)$n") => s.copy(children = n.toInt)
        case (s, r"cats: (.+)$n") => s.copy(cats = n.toInt)
        case (s, r"samoyeds: (.+)$n") => s.copy(samoyeds = n.toInt)
        case (s, r"pomeranians: (.+)$n") => s.copy(pomeranians = n.toInt)
        case (s, r"akitas: (.+)$n") => s.copy(akitas = n.toInt)
        case (s, r"vizslas: (.+)$n") => s.copy(vizslas = n.toInt)
        case (s, r"goldfish: (.+)$n") => s.copy(goldfish = n.toInt)
        case (s, r"trees: (.+)$n") => s.copy(trees = n.toInt)
        case (s, r"cars: (.+)$n") => s.copy(cars = n.toInt)
        case (s, r"perfumes: (.+)$n") => s.copy(perfumes = n.toInt)
      }
  }

  val input = Source.fromFile("src/main/scala/aoc2015/day16.txt").getLines.toList
  val sues = input map parse
  sues foreach println

  val sueFacts =
    """children: 3
      |cats: 7
      |samoyeds: 2
      |pomeranians: 3
      |akitas: 0
      |vizslas: 0
      |goldfish: 5
      |trees: 3
      |cars: 2
      |perfumes: 1""".stripMargin.lines.toList

  def testSue1(sue: Sue): Boolean = {
    val sueChecks = sueFacts.map {
      case r"children: (.+)$n" => sue.children == n.toInt || sue.children == Int.MaxValue
      case r"cats: (.+)$n" => sue.cats == n.toInt || sue.cats == Int.MaxValue
      case r"samoyeds: (.+)$n" => sue.samoyeds == n.toInt || sue.samoyeds == Int.MaxValue
      case r"pomeranians: (.+)$n" => sue.pomeranians == n.toInt || sue.pomeranians == Int.MaxValue
      case r"akitas: (.+)$n" => sue.akitas == n.toInt || sue.akitas == Int.MaxValue
      case r"vizslas: (.+)$n" => sue.vizslas == n.toInt || sue.vizslas == Int.MaxValue
      case r"goldfish: (.+)$n" => sue.goldfish == n.toInt || sue.goldfish == Int.MaxValue
      case r"trees: (.+)$n" => sue.trees == n.toInt || sue.trees == Int.MaxValue
      case r"cars: (.+)$n" => sue.cars == n.toInt || sue.cars == Int.MaxValue
      case r"perfumes: (.+)$n" => sue.perfumes == n.toInt || sue.perfumes == Int.MaxValue
    }
    sueChecks.forall(_ == true)
  }

  def testSue2(sue: Sue): Boolean = {
    val sueChecks = sueFacts.map {
      case r"children: (.+)$n" => sue.children == n.toInt || sue.children == Int.MaxValue
      case r"cats: (.+)$n" => sue.cats > n.toInt || sue.cats == Int.MaxValue
      case r"samoyeds: (.+)$n" => sue.samoyeds == n.toInt || sue.samoyeds == Int.MaxValue
      case r"pomeranians: (.+)$n" => sue.pomeranians < n.toInt || sue.pomeranians == Int.MaxValue
      case r"akitas: (.+)$n" => sue.akitas == n.toInt || sue.akitas == Int.MaxValue
      case r"vizslas: (.+)$n" => sue.vizslas == n.toInt || sue.vizslas == Int.MaxValue
      case r"goldfish: (.+)$n" => sue.goldfish < n.toInt || sue.goldfish == Int.MaxValue
      case r"trees: (.+)$n" => sue.trees > n.toInt || sue.trees == Int.MaxValue
      case r"cars: (.+)$n" => sue.cars == n.toInt || sue.cars == Int.MaxValue
      case r"perfumes: (.+)$n" => sue.perfumes == n.toInt || sue.perfumes == Int.MaxValue
    }
    sueChecks.forall(_ == true)
  }

  val rightSues1 = sues.filter(testSue1)
  println()
  println(s"Found Part1:")
  rightSues1 foreach println

  val rightSues2 = sues.filter(testSue2)
  println()
  println(s"Found Part2:")
  rightSues2 foreach println


}
