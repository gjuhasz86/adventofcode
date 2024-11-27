package aoc2016

object Day1 extends App {
  val input = "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1"
  //  val input = "R8, R4, R4, R8"

  sealed trait Dir
  case object L extends Dir
  case object R extends Dir
  case object I extends Dir

  def parseDir(c: Char) = c match {
    case 'L' => L
    case 'R' => R
    case x => throw new IllegalArgumentException(s"Unexpected [$x]")
  }

  implicit class TurnSeq[T](val seq: Seq[T]) extends AnyVal {

    def left(n: Int): Seq[T] = if (n == 0) seq else seq.l1.left(n - 1)
    def right(n: Int): Seq[T] = if (n == 0) seq else seq.r1.right(n - 1)

    def l1: Seq[T] = seq match {
      case x +: xs => xs :+ x
    }

    def r1: Seq[T] = seq match {
      case xs :+ x => x +: xs
    }
  }

  val dirs = Seq(N, W, S, E)
  sealed trait Face {
    def neighbors = dirs.left(dirs.indexOf(this))
    def left: Face = neighbors.l1.head
    def right: Face = neighbors.r1.head
    def turn(d: Dir): Face = d match {
      case L => left
      case R => right
      case I => this
    }
  }
  case object N extends Face
  case object E extends Face
  case object S extends Face
  case object W extends Face

  val parsed: Seq[(Dir, Int)] = input.split(", ").map(str => (parseDir(str.head), str.drop(1).toInt))

  println(parsed)
  val steps = parsed.flatMap { case (d, i) => d +: (2 to i).map(_ => I) }

  println(steps.mkString)

  case class Pos(r: Int, c: Int, face: Face) {
    def forward = face match {
      case N => this.copy(r = r - 1)
      case S => this.copy(r = r + 1)
      case W => this.copy(c = c - 1)
      case E => this.copy(c = c + 1)
    }
    def move(d: Dir): Pos = d match {
      case L | R => this.copy(face = face.turn(d)).forward
      case I => this.forward
    }
  }

  def moveAll(pos: Pos, steps: Seq[Dir]): Seq[Pos] = steps match {
    case Seq() => Seq(pos)
    case h +: t => pos +: moveAll(pos.move(h), t)
  }

  val allPos = moveAll(Pos(0, 0, N), steps)
  allPos foreach println

  type Pos0 = (Int, Int)
  def findDup(visited: Seq[Pos0], rest: Seq[Pos0]): Option[Pos0] = rest match {
    case Seq() => None
    case h +: t => if (visited.contains(h)) Some(h) else findDup(visited :+ h, t)
  }

  val allPos0 = allPos.map(p => (p.r, p.c))
  val dup = findDup(Seq(), allPos0)
  println(s"found dup: $dup")
  println(dup.map(d => d._1 + d._2))
}
