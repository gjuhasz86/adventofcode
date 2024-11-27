package aoc2015

import scala.util.Try

object Day15v2 extends App {
  implicit class RegexFoo(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)


  val input =
    """Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
      |PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
      |Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
      |Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8""".stripMargin.lines.toList

  val input1 =
    """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin.lines.toList

  def parse(str: String): Ingredient = str match {
    case r"(.+)$name: capacity (.+)$c, durability (.+)$d, flavor (.+)$f, texture (.+)$t, calories (.+)$cal" =>
      Ingredient(name, c.toInt, d.toInt, f.toInt, t.toInt, cal.toInt)
  }

  val ingredients = input map parse

  ingredients foreach println

  case class Cookie(ins: Map[Ingredient, Int]) extends AnyVal {
    private def scoreBy(f: Ingredient => Int): Int = {
      val score = ins.map({ case (in, am) => f(in) * am }).sum
      if (score < 0) 0 else score
    }

    def capacity: Int = scoreBy(_.capacity)
    def durability: Int = scoreBy(_.durability)
    def flavor: Int = scoreBy(_.flavor)
    def texture: Int = scoreBy(_.texture)
    def calories: Int = scoreBy(_.calories)
    def total: Int = capacity * durability * flavor * texture


    def size: Int = ins.values.sum
    def add(in: Ingredient): Cookie = Cookie(ins + (in -> (ins(in) + 1)))
    def rem(in: Ingredient): Cookie = Cookie(ins + (in -> (ins(in) - 1)))
    def addMore(in: Ingredient, am: Int): Cookie = Cookie(ins + (in -> am))

    override def toString: String = {
      val insStr = ins.map(e => s"${e._1.name}:${e._2}").mkString(", ")
      s"Cookie[$size][t:$total, c:$calories]($insStr)"
    }
  }
  val EmptyCookie: Cookie = Cookie(Map.empty[Ingredient, Int].withDefaultValue(0))


  var count = 0
  def brute(ins: List[Ingredient], cookie: Cookie): Cookie = ins match {
    case h :: t =>
      val cookies = for (i <- 0 to (100 - cookie.size)) yield brute(t, cookie.addMore(h, i))
      Try {
        cookies.filter(_.calories == 500).maxBy(_.total)
      }.getOrElse(EmptyCookie)
    case Nil =>
      count = count + 1
      if (count % 100000 == 0) println(s"[$count]: $cookie")
      cookie
  }

  val best = brute(ingredients, EmptyCookie)

  println(s"Best: $best")
}
