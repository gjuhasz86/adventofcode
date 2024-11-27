package aoc2015

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

object Day19v3 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }


  val input = Source.fromFile("src/main/scala/aoc2015/day19.txt").getLines.toList

  val moleculeRe = """(?=[A-Z])"""

  trait Symbol {
    def name: String
    def next(i: Int): Symbol
  }
  case class T(name: String) extends Symbol {
    override def toString: String = s"_$name"
    override def next(i: Int) = ???
  }
  case class Nt(name: String) extends Symbol {
    override def toString: String = name
    override def next(i: Int) = name match {
      case r"([A-Z][a-z]?)$n[0-9]*" => Nt(n + i)
    }
  }

  case class Rule(lhs: Symbol, rhs: List[Symbol]) {
    def normalized: List[Rule] = Rule.normalize(this)

    override def toString: String = s"$lhs -> ${rhs.mkString(" ")}"
  }

  object Rule {
    var id = 0
    def nextId() = {
      id = id + 1
      id
    }

    def normalize(rule: Rule): List[Rule] = rule.rhs match {
      case head :: tail if rule.rhs.size > 2 =>
        val newLhs = rule.lhs.next(nextId())
        val r1 = Rule(rule.lhs, head :: newLhs :: Nil)
        val rest = normalize(Rule(newLhs, tail))
        r1 :: rest
      case _ => rule :: Nil
    }
  }

  def parse(str: String): Rule = str match {
    case r"(.+)$lhs => (.+)$rhs" => Rule(Nt(lhs), rhs.split(moleculeRe).map(Nt.apply).toList)
  }

  val rules = input.dropRight(2) map parse
  complete(rules).flatMap(_.normalized) foreach println

  def complete(rs: List[Rule]): List[Rule] = {
    val syms = (rs.map(_.lhs) ::: rs.flatMap(_.rhs)).distinct
    rs ::: syms.map(sym => Rule(sym, T(sym.name) :: Nil))
  }

  val goal = input.last.split(moleculeRe).map(T.apply)
  println(goal.mkString(" "))

}
