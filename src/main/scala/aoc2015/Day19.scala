package aoc2015

import common.search.ASearch
import common.search.ASearch.State

import scala.io.Source

object Day19 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val input1 =
    """H => HO
      |H => OH
      |O => HH
      |
      |HOH""".stripMargin.lines.toList

  val input2 =
    """e => H
      |e => O
      |H => HO
      |H => OH
      |O => HH
      |
      |HOHOHO""".stripMargin.lines.toList

  val input = Source.fromFile("src/main/scala/aoc2015/day19.txt").getLines.toList

  case class Rule(from: String, to: String) {
    def use(str: String): List[String] = use0(str, 0, Nil)
    private def use0(str: String, n: Int, acc: List[String]): List[String] = {
      val idx = str.indexOf(from, n)
      if (idx == -1) {
        acc
      } else {
        val applied = str.take(idx) + to + str.drop(idx + from.length)
        use0(str, idx + 1, applied :: acc)
      }
    }

    def range(n: Int): Range = n until n + from.length

    def allRanges(str: String): List[Range] =
      allIndices(str).map(range)

    def allIndices(str: String): List[Int] =
      (0 until str.length).filter(str.startsWith(from, _)).toList

    def reversed: Rule = Rule(to, from)
  }

  def parse(str: String): Rule = str match {
    case r"(.+)$f => (.+)$t" => Rule(f, t)
  }

  val rules = input.dropRight(2).map(parse)
  val medicine = input.last

  //  rules foreach println
  //  println()
  //  val res = rules.flatMap(_.use(medicine)).distinct
  //  res foreach println
  //  println(res.size)


  case class MoleculeState(str: String)(goalStr: String, override val cost: Int) extends State {
    override type OutState = MoleculeState
    override type Move = String

    override def moves: List[Move] = rules.flatMap(_.reversed.use(str)).distinct

    override def next(m: Move): MoleculeState = MoleculeState(m)(goalStr, cost + 1)

    override def valid: Boolean = true
    override def goal: Boolean = str == goalStr
    override def estToGoal: Int = 0
  }


  //  val res2 = ASearch.search(MoleculeState(medicine)("e", 0), debug = true, debugN = 500)
  //  res2 foreach println
  //  println(res2.head.pathLenght)

  case class Overlap(str: String, map: Map[Int, List[Rule]], inits: Set[Int]) {

    def incAll(rules: List[Rule]): Overlap =
      rules.foldLeft(this)((ol, rule) => ol.inc(rule))

    def inc(rule: Rule): Overlap =
      rule.allRanges(str).foldLeft(this)((ol, range) => ol.inc(range, rule))


    private def inc(range: Range, rule: Rule): Overlap =
      range.foldLeft(this)((acc, n) => acc.inc(n, rule))
        .addInit(range.head)

    private def inc(n: Int, rule: Rule): Overlap = Overlap(str, map + (n -> (rule :: map(n))), inits)

    def addInit(n: Int): Overlap = this.copy(inits = inits + n)


    def uniqRules: List[(Rule, Int)] = {
      inits.toList
        .filter(n => sizeAt(n) == 1)
        .map(n => (at(n).head, n))
        .filter { case (rule, n) => getSize(rule.range(n)).forall(_ == 1) }
    }

    def at(n: Int): List[Rule] = map(n)

    def sizeAt(n: Int): Int = at(n).size
    def getSize(range: Range): List[Int] = range.toList.map(sizeAt)

    def hasInitAt(n: Int): Boolean = inits.contains(n)

    def initMarkerAt(n: Int): String = if (hasInitAt(n)) "*" else " "
    def p(range: Range): String =
      range.mkString("\t") + "\n" +
        getSize(range).mkString("\t") + "\n" +
        range.map(initMarkerAt).mkString("\t")

    def pp: Unit = println(str.mkString("\t") + "\n" + p(0 until str.length))
  }
  object Overlap {
    def of(str: String): Overlap = Overlap(str, Map.empty[Int, List[Rule]].withDefaultValue(Nil), Set.empty[Int])
  }

  //  def findOverlaps(str: String, rules: List[Rule]): Overlap = {
  //    rules.flatMap(_.allRanges(str)).foldLeft(Overlap.empty)((ol, r) => ol.inc(r))
  //  }

  //  val overlaps = findOverlaps(medicine, rules.map(_.reversed))
  //  overlaps.pp(medicine)

  val ol = Overlap.of(medicine).incAll(rules.map(_.reversed))
  ol.pp
  ol.uniqRules.sortBy(_._2) foreach println
  println()
  ol.map.toList.sortBy(_._1) foreach println
}
