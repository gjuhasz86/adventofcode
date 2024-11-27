package aoc2016

import scala.annotation.tailrec
import scala.util.Try

/*
--- Day 23: Safe Cracking ---

This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's private office is here, complete with a safe hidden behind a painting, and who wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry. A sticky note attached to the safe has a password hint on it: "eggs". The painting is of a large rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display; instead, the keypad comes apart in your hands, apparently having been smashed. Behind it is some kind of socket - one that matches a connector in your prototype computer! You pull apart the smashed keypad and extract the logic circuit, plug it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the safe. You extract the assembunny code from the logic chip (your puzzle input).
The code looks like it uses almost the same architecture and instruction set that the monorail computer used! You should be able to use the same assembunny interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):

For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
The arguments of a toggled instruction are not affected.
If an attempt is made to toggle an instruction outside the program, nothing happens.
If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.
For example, given this program:

cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a

cpy 2 a initializes register a to 2.
The first tgl a toggles an instruction a (2) away from it, which changes the third tgl a into inc a.
The second tgl a also modifies an instruction 2 away from it, which changes the cpy 1 a into jnz 1 a.
The fourth line, which is now inc a, increments a to 3.
Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead, skipping the dec a instructions.
In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry (the number of eggs, 7) in register a, run the code, and then send the value left in register a to the safe.

What value should be sent to the safe?


 */
object Day23 extends App {

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
  case class Tgl(target: Reg) extends Op
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
    case Array("tgl", t) => Tgl(Reg(t))
  }

  case class Machine(code: Vector[Op], mem: Map[String, Int] = Map.empty.withDefaultValue(0), p: Int = 0, i: Int = 0) {
    implicit val iMem: Map[String, Int] = mem


    def stopped: Boolean = !valid(p)

    def stepOne: Machine = code.slice(p, p + 6) match {
      case vec@Vector(Cpy(b, c1@Reg(c)), Inc(a), Dec(c2), Jnz(c3, Lit(-2)), Dec(d1), Jnz(d2, Lit(-5)))
        if c1 == c2 && c2 == c3 && d1 == d2 =>
        println("OPTIMIZED")
        set(a.name, a.get + b.get * d1.get).set(c, 0).set(d1.name, 0).next(6)
      case vec => run(vec.head)
    }

    def run(op: Op): Machine = op match {
      case Inc(Reg(r)) => set(r, mem(r) + 1).next
      case Dec(Reg(r)) => set(r, mem(r) - 1).next
      case Jnz(tRef, nRef) => if (tRef.get == 0) next else next(nRef.get)
      case Cpy(ref, Reg(r)) => set(r, ref.get).next
      case Cpy(ref, Lit(r)) => next
      case Tgl(nRef) => toggle(nRef.get).next
    }


    def valid(p0: Int): Boolean = 0 <= p0 && p0 < code.size

    def toggle(n: Int): Machine =
      if (valid(p + n))
        this.copy(code = code.updated(n + p, toggle(code(n + p))))
      else
        this

    def toggle(op: Op): Op = op match {
      case Inc(t) => Dec(t)
      case Dec(t) => Inc(t)
      case Tgl(t) => Inc(t)
      case Cpy(s, t) => Jnz(s, t)
      case Jnz(s, t) => Cpy(s, t)
    }

    def set(r: String, v: Int): Machine = Machine(code, mem + (r -> v), p, i + 1)
    def next: Machine = Machine(code, mem, p + 1, i + 1)
    def next(n: Int): Machine = Machine(code, mem, p + n, i + 1)
    def run: Machine = Machine.run(this)

    override def toString: String = {

      val codeStr0 = code.zipWithIndex.map {
        case (o, i) if i == p => s" * $o"
        case (o, i) => s" $i $o"
      }
      val codeStr = codeStr0.mkString(" ; ")
      s"$i: [$p] | $codeStr\n$mem\n"
    }
  }
  object Machine {
    @tailrec final def run(m: Machine): Machine = {
      println(m)
      if (m.stopped)
        m
      else
        run(m.stepOne)
    }
  }

  val input =
    """cpy a b
      |dec b
      |cpy a d
      |cpy 0 a
      |cpy b c
      |inc a
      |dec c
      |jnz c -2
      |dec d
      |jnz d -5
      |dec b
      |cpy b c
      |cpy c d
      |dec d
      |inc c
      |jnz d -2
      |tgl c
      |cpy -16 c
      |jnz 1 c
      |cpy 72 c
      |jnz 77 d
      |inc a
      |inc d
      |jnz d -2
      |inc c
      |jnz c -5""".stripMargin.lines.toVector

  val input1 =
    """cpy 2 a
      |tgl a
      |tgl a
      |tgl a
      |cpy 1 a
      |dec a
      |dec a""".stripMargin.lines.toVector

  val sourceCode = input map parse
  sourceCode.zipWithIndex.map(_.swap) foreach println

  val machine = Machine(sourceCode, Map("a" -> 12).withDefaultValue(0))

  val endState = machine.run
  println(Int.MaxValue)
  //  println(endState)
  //479007144
}
