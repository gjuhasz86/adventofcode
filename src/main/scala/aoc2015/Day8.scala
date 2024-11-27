package aoc2015

import scala.io.Source

object Day8 extends App {

  val input = Source.fromFile("src/main/scala/aoc2015/day8.txt").getLines().toList


  val escaped = input.map { str =>
    str.drop(1)
      .dropRight(1)
      .replaceAll("""[\\]["]""", "\"")
      .replaceAll("""[\\][\\]""", "\\\\")
      .replaceAll("""[\\]x[0-9a-f][0-9a-f]""", "_")
  }


  val unescaped = input.map { str =>
    '"' + str.replaceAll("""[\\]""", "\\\\\\\\")
      .replaceAll("""[\"]""", "\\\\\"") + '"'
  }


  println(escaped)
  val memLenght = escaped.map(_.length).sum

  println()

  println(unescaped)
  val unescLen = unescaped.map(_.length).sum

  println(memLenght)
  println(unescLen)

  val codeLenght = input.map(_.length).sum
  println(codeLenght)

  println()
  println(codeLenght - memLenght)
  println(unescLen - codeLenght)


}
