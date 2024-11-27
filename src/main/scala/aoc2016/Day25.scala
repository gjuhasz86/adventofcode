package aoc2016

import scala.annotation.tailrec
import scala.util.Try

object Day25 extends App {

  sealed trait Ref {
    def get(implicit mem: Map[String, Int]): Int = this match {
      case Reg(r) => mem(r)
      case Lit(n) => n
    }
  }
  case class Lit(n: Int) extends Ref {
    override def toString: String = n.toString
  }
  case class Reg(name: String) extends Ref {
    override def toString: String = name
  }

  sealed trait Op
  case class Inc(target: Reg) extends Op
  case class Dec(target: Reg) extends Op
  case class Out(target: Reg) extends Op
  case class Cpy(src: Ref, target: Ref) extends Op
  case class Jnz(target: Ref, n: Ref) extends Op

  def parse(str: String): Op = str.split(" ") match {
    case Array("cpy", s, t) =>
      val srcRef = Try(Lit(s.toInt)).getOrElse(Reg(s))
      Cpy(srcRef, Reg(t))
    case Array("inc", t) => Inc(Reg(t))
    case Array("dec", t) => Dec(Reg(t))
    case Array("jnz", t, n) =>
      val tRef = Try(Lit(t.toInt)).getOrElse(Reg(t))
      val nRef = Try(Lit(n.toInt)).getOrElse(Reg(n))
      Jnz(tRef, nRef)
    case Array("out", t) => Out(Reg(t))
  }

  def alternating(list: List[Int]): Boolean = list match {
    case h1 :: h2 :: rest => h1 != h2
    case _ => true
  }

  case class Machine(code: Vector[Op], mem: Map[String, Int] = Map.empty.withDefaultValue(0), limit: Int, output: List[Int] = Nil, p: Int = 0, i: Int = 0) {
    implicit val iMem: Map[String, Int] = mem


    def stopped: Boolean = !valid(p) || !alternating(output) || i > 100000

    def stepOne: Machine = code.slice(p, p + 6) match {
      case vec@Vector(Cpy(b, c1@Reg(c)), Inc(a), Dec(c2), Jnz(c3, Lit(-2)), Dec(d1), Jnz(d2, Lit(-5)))
        if c1 == c2 && c2 == c3 && d1 == d2 =>
        //        println("OPTIMIZED")
        set(a.name, a.get + b.get * d1.get).set(c, 0).set(d1.name, 0).next(6)
      case vec => run(vec.head)
    }

    def run(op: Op): Machine = op match {
      case Inc(Reg(r)) => set(r, mem(r) + 1).next
      case Dec(Reg(r)) => set(r, mem(r) - 1).next
      case Jnz(tRef, nRef) => if (tRef.get == 0) next else next(nRef.get)
      case Cpy(ref, Reg(r)) => set(r, ref.get).next
      case Cpy(ref, Lit(r)) => next
      case Out(nRef) => out(nRef.get).next
    }


    def valid(p0: Int): Boolean = 0 <= p0 && p0 < code.size

    def out(n: Int): Machine = this.copy(output = n :: output)
    def set(r: String, v: Int): Machine = Machine(code, mem + (r -> v), limit, output, p, i + 1)
    def next: Machine = Machine(code, mem, limit, output, p + 1, i + 1)
    def next(n: Int): Machine = Machine(code, mem, limit, output, p + n, i + 1)
    def run: Machine = Machine.run(this)

    override def toString: String = {

      val codeStr0 = code.zipWithIndex.map {
        case (o, i) if i == p => s" * $o"
        case (o, i) => s" $i $o"
      }
      val codeStr = codeStr0.mkString(" ; ")
      s"$i: [$p][${code(p)}] | $codeStr\n$mem\n$output\n"
    }
  }
  object Machine {
    @tailrec final def run(m: Machine): Machine = {
      //      println(m)
      if (m.stopped)
        m
      else
        run(m.stepOne)
    }
  }

  val input =
    """cpy a d
      |cpy 9 c
      |cpy 282 b
      |inc d
      |dec b
      |jnz b -2
      |dec c
      |jnz c -5
      |cpy d a
      |jnz 0 0
      |cpy a b
      |cpy 0 a
      |cpy 2 c
      |jnz b 2
      |jnz 1 6
      |dec b
      |dec c
      |jnz c -4
      |inc a
      |jnz 1 -7
      |cpy 2 b
      |jnz c 2
      |jnz 1 4
      |dec b
      |dec c
      |jnz 1 -4
      |jnz 0 0
      |out b
      |jnz a -19
      |jnz 1 -21""".stripMargin.lines.toVector

  val sourceCode = input map parse
  sourceCode.zipWithIndex.map(_.swap) foreach println

  def find(a: Int, limit: Int): Machine = {
    println(s"Trying [$a]...")
    val machine = Machine(sourceCode, Map("a" -> a).withDefaultValue(0), limit)
    val endState = machine.run
    println(s"[${endState.i}] ${endState.output}")
    println()
    if (!alternating(endState.output))
      find(a + 1, limit)
    else
      endState
  }

  find(0, 100000)
}

