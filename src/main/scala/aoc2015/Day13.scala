package aoc2015

import common.search.ASearch
import common.search.ASearch.State

import scala.io.Source


// test output: 330

/*
   +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
   -7  +83
 */

object Day13 extends App {

  val input = Source.fromFile("src/main/scala/aoc2015/day13.txt").getLines.toList
  val input0 =
    """Alice would gain 54 happiness units by sitting next to Bob.
      |Alice would lose 79 happiness units by sitting next to Carol.
      |Alice would lose 2 happiness units by sitting next to David.
      |Bob would gain 83 happiness units by sitting next to Alice.
      |Bob would lose 7 happiness units by sitting next to Carol.
      |Bob would lose 63 happiness units by sitting next to David.
      |Carol would lose 62 happiness units by sitting next to Alice.
      |Carol would gain 60 happiness units by sitting next to Bob.
      |Carol would gain 55 happiness units by sitting next to David.
      |David would gain 46 happiness units by sitting next to Alice.
      |David would lose 7 happiness units by sitting next to Bob.
      |David would gain 41 happiness units by sitting next to Carol.""".stripMargin.lines.toList

  def parse(str: String): Relation = {
    val parts = str.split(" ")
    val mul = if (parts(2) == "gain") 1 else -1
    Relation(parts(0), parts(10).dropRight(1), parts(3).toInt * mul)
  }

  case class Relation(p1: String, p2: String, happines: Int)

  val relations0 = input map parse
  relations0 foreach println

  val names = relations0.map(_.p1).distinct
  val relations = relations0 ++ names.flatMap(name => List(Relation("me", name, 0), Relation(name, "me", 0)))

  def evaluate(table: List[String]): Int = {
    val pairs = table zip (table.tail :+ table.head)
    val hap1 = pairs.map(pair => relations.find(r => r.p1 == pair._1 && r.p2 == pair._2).get.happines)
    val hap2 = pairs.map(pair => relations.find(r => r.p1 == pair._2 && r.p2 == pair._1).get.happines)
    //    pairs zip hap2 zip hap2 foreach println
    hap1.sum + hap2.sum
  }

  case class TableState(people: List[String], allNames: List[String]) extends State {
    override type OutState = TableState
    override type Move = String
    override def moves: List[String] = allNames
    override def next(m: String): TableState = TableState(m :: people, allNames)
    override def valid: Boolean = people.size == people.distinct.size
    override def goal: Boolean = people.size == allNames.size
    override def cost: Int = 1
    override def estToGoal: Int = 0

    def score = evaluate(people)
  }

  def solve(allNames: List[String]) = {
    val allCombs = ASearch.search(TableState("Alice" :: Nil, allNames), true, true).map(_.state)
    val part1Sol = allCombs.maxBy(_.score)
    println(part1Sol)
    part1Sol.score
  }

  val part1Res = solve(names)

  val namesPart2 = relations.map(_.p1).distinct
  val part2Res = solve(namesPart2)

  println(s"PART1: $part1Res")
  println(s"PART2: $part2Res")

}
