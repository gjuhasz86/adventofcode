package aoc2016

import scala.util.Try
import scala.collection.immutable.Map

object Day12 extends App {

  val input =
    """cpy 1 a
      |cpy 1 b
      |cpy 26 d
      |jnz c m
      |cpy 7 c
      |inc d
      |dec c
      |jnz c -2
      |cpy a c
      |inc a
      |dec b
      |jnz b -2
      |cpy c b
      |dec d
      |jnz d -6
      |cpy 14 c
      |cpy 14 d
      |inc a
      |dec d
      |jnz d -2
      |dec c
      |jnz c -5""".stripMargin.lines.toList


  val input1 = List(
    "cpy 3 b",
    "cpy 4 a",
    "inc a",
    "dec b",
    "jnz b -2")

  sealed trait Ref
  case class Lit(n: Int) extends Ref
  case class Reg(name: String) extends Ref

  sealed trait Op
  case class Cpy(src: Ref, target: Reg) extends Op
  case class Inc(target: Reg) extends Op
  case class Dec(target: Reg) extends Op
  case class Jnz(target: Ref, n: Int) extends Op
  case class Add(src: Reg, target: Reg) extends Op
  case object Nop extends Op

  def parse(str: String): Op = str.split(" ") match {
    case Array("cpy", s, t) =>
      val srcRef = Try(Lit(s.toInt)).getOrElse(Reg(s))
      Cpy(srcRef, Reg(t))
    case Array("inc", t) => Inc(Reg(t))
    case Array("dec", t) => Dec(Reg(t))
    case Array("jnz", t, n) =>
      val tRef = Try(Lit(t.toInt)).getOrElse(Reg(t))
      Jnz(tRef, n.toInt)
  }

  val sourceCode = input map parse
  sourceCode.zipWithIndex.map(_.swap) foreach println

  case class Machine(p: Int, mem: Map[String, Int]) {
    def run(op: Op): Machine = op match {
      case Cpy(Lit(v), Reg(r)) => set(r, v).next
      case Cpy(Reg(sr), Reg(tr)) => set(tr, mem(sr)).next
      case Inc(Reg(r)) => set(r, mem(r) + 1).next
      case Dec(Reg(r)) => set(r, mem(r) - 1).next
      case Jnz(Lit(v), n) => if (v == 0) next else next(n)
      case Jnz(Reg(r), n) => if (mem(r) == 0) next else next(n)
      case Add(Reg(s), Reg(t)) => set(t, mem(s) + mem(t)).next
      case Nop => next
    }

    def set(r: String, v: Int): Machine = Machine(p, mem + (r -> v))
    def next: Machine = Machine(p + 1, mem)
    def next(n: Int): Machine = Machine(p + n, mem)
  }

  def optimize(ops: List[Op]): List[Op] = ops match {
    case Nil => Nil
    case Inc(Reg(a)) :: Dec(Reg(b)) :: Jnz(Reg(c), -2) :: rest if b == c =>
      Add(Reg(b), Reg(a)) :: Cpy(Lit(0), Reg(b)) :: Nop :: optimize(rest)
    case h :: t => h :: optimize(t)
  }

  def run(machine: Machine, code: List[Op]): Machine = code.lift(machine.p) match {
    case None => machine
    case Some(op) =>
      println(machine)
      run(machine.run(op), code)
  }

  println()
  val optimizedCode = optimize(sourceCode)
  optimizedCode.zipWithIndex.map(_.swap) foreach println
  println()

  val endStatePart1 = run(Machine(0, Map.empty.withDefaultValue(0)), optimizedCode)
  println(endStatePart1)

  println()

  val endStatePart2 = run(Machine(0, Map("c" -> 1).withDefaultValue(0)), optimizedCode)
  println(endStatePart2)

  println()
  println(endStatePart1)
  println(endStatePart2)

  val resPart1 = endStatePart1.mem("a")
  val resPart2 = endStatePart2.mem("a")

  println()
  println(s"PART1: $resPart1")
  println(s"PART2: $resPart2")

}
