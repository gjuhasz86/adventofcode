package aoc2015

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Try

object Day7 extends App {

  val input = Source.fromFile("src/main/scala/aoc2015/day7.txt").getLines().toList

  //  val input =
  //    """123 -> x
  //      |456 -> y
  //      |x AND y -> d
  //      |x OR y -> e
  //      |x LSHIFT 2 -> f
  //      |y RSHIFT 2 -> g
  //      |NOT x -> h
  //      |NOT y -> i""".stripMargin.lines.toList

  sealed abstract class Gate(val inWires: List[String]) {
    def step(wires: Map[String, Int]): Map[String, Int]
    def out: String
  }
  case class Direct(in: String, override val out: String) extends Gate(in :: Nil) {
    def get(ws: Map[String, Int]) = Try(in.toInt).orElse(Try(ws(in)))
    override def step(ws: Map[String, Int]) = get(ws).map { in => ws + (out -> in) } getOrElse ws
  }
  case class And(in1: String, in2: String, override val out: String) extends Gate(in1 :: in2 :: Nil) {
    def get1(ws: Map[String, Int]) = Try(in1.toInt).orElse(Try(ws(in1)))
    def get2(ws: Map[String, Int]) = Try(in2.toInt).orElse(Try(ws(in2)))
    def get(ws: Map[String, Int]) = Try {
      (get1(ws).get, get2(ws).get)
    }
    override def step(ws: Map[String, Int]) = get(ws).map { case (i1, i2) =>
      ws + (out -> (i1 & i2))
    } getOrElse ws
  }
  case class Or(in1: String, in2: String, override val out: String) extends Gate(in1 :: in2 :: Nil) {
    def get1(ws: Map[String, Int]) = Try(in1.toInt).orElse(Try(ws(in1)))
    def get2(ws: Map[String, Int]) = Try(in2.toInt).orElse(Try(ws(in2)))
    def get(ws: Map[String, Int]) = Try {
      (get1(ws).get, get2(ws).get)
    }
    override def step(ws: Map[String, Int]) = get(ws).map { case (i1, i2) =>
      ws + (out -> (i1 | i2))
    } getOrElse ws
  }
  case class LShift(in: String, n: Int, override val out: String) extends Gate(in :: Nil) {
    def get(ws: Map[String, Int]) = Try(in.toInt).orElse(Try(ws(in)))
    override def step(ws: Map[String, Int]) = get(ws).map { in => ws + (out -> ((in << n) & 65535)) } getOrElse ws
  }
  case class RShift(in: String, n: Int, override val out: String) extends Gate(in :: Nil) {
    def get(ws: Map[String, Int]) = Try(in.toInt).orElse(Try(ws(in)))
    override def step(ws: Map[String, Int]) = get(ws).map { in => ws + (out -> ((in >> n) & 65535)) } getOrElse ws
  }
  case class Not(in: String, override val out: String) extends Gate(in :: Nil) {
    def get(ws: Map[String, Int]) = Try(in.toInt).orElse(Try(ws(in)))
    override def step(ws: Map[String, Int]) = get(ws).map { in => ws + (out -> ((~in) & 65535)) } getOrElse ws
  }

  val Nullary = """([^ ]+?) -> ([^ ]+?)""".r
  val Unary = """([^ ]+?) ([^ ]+?) -> ([^ ]+?)""".r
  val Binary = """([^ ]+?) ([^ ]+?) ([^ ]+?) -> ([^ ]+?)""".r

  def parse(str: String): Gate = str match {
    case Nullary(in, out) => Direct(in, out)
    case Unary(op, in, out) if op == "NOT" => Not(in, out)
    case Binary(in, op, n, out) if op == "LSHIFT" => LShift(in, n.toInt, out)
    case Binary(in, op, n, out) if op == "RSHIFT" => RShift(in, n.toInt, out)
    case Binary(in1, op, in2, out) if op == "AND" => And(in1, in2, out)
    case Binary(in1, op, in2, out) if op == "OR" => Or(in1, in2, out)
  }

  val parsed = input.map(parse)
  parsed foreach println

  val wires = parsed.flatMap(_.inWires).distinct.sorted
  println(wires)

  def solve(ws: Map[String, Int], gates: List[Gate]): Map[String, Int] = {
    val newWs = gates.map(_.step(ws)).foldLeft(Map[String, Int]())(_ ++ _)
    if (newWs.lift("a").isDefined) {
      newWs
    } else if (newWs == ws) {
      println("No solution...")
      newWs
    } else {
      println(newWs)
      solve(newWs, gates)
    }
  }

  def runSolve(gates: List[Gate]): Int = {

    val signals = solve(Map(), gates)
    signals.toList.sortBy(_._1).map(x => s"${x._1}: ${x._2}") foreach println

    val keys = signals.keys.toList
    println(keys)

    val solvedGates = gates.filter(_.inWires.forall(x => keys.contains(x)))
    val solvedWires = gates.filter(_.inWires.forall(x => keys.contains(x))).map(_.out)
    println(solvedGates)
    println(solvedWires)

    val a = signals("a")
    println(s"a = $a")
    a
  }


  val a = runSolve(parsed)

  val newGates = parsed.collect {
    case Direct(_, "b") => Direct(a.toString, "b")
    case g => g
  }

  val a2 = runSolve(newGates)

  println(s"a = $a")
  println(s"a2 = $a2")

  //  def findRoute(acc: List[String], from: List[String], to: String): List[String] = {
  //    from match {
  //      case Nil => acc
  //      case h :: t if h == to => acc :+ h
  //      case h :: t =>
  //        val gates = parsed.filter(_.inWires.contains(h))
  //        val outs = gates.map(_.out)
  //        findRoute(acc :+ h, t ::: outs, to)
  //    }
  //  }
  //
  //  val route = findRoute(Nil, "c" :: Nil, "a")
  //  println(route)
}
