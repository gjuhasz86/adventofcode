package aoc2015

import common.genetic.{Entity, EntityFactory, Genetic}
import common.search.ASearch
import common.search.ASearch.State

import scala.util.Random

/*
--- Day 15: Science for Hungry People ---

Today, you set out on the task of perfecting your milk-dunking cookie recipe. All you have to do is find the right balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a list of the remaining ingredients you could use to finish the recipe (your puzzle input) and their properties per teaspoon:

capacity (how well it helps the cookie absorb milk)
durability (how well it keeps the cookie intact when full of milk)
flavor (how tasty it makes the cookie)
texture (how it improves the feel of the cookie)
calories (how many calories it adds to the cookie)
You can only measure ingredients in whole-teaspoon amounts accurately, and you have to be accurate so you can reproduce your results in the future. The total score of a cookie can be found by adding up each of the properties (negative totals become 0) and then multiplying together everything except calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon (because the amounts of each ingredient must add up to 100) would result in a cookie with the following properties:

A capacity of 44*-1 + 56*2 = 68
A durability of 44*-2 + 56*3 = 80
A flavor of 44*6 + 56*-2 = 152
A texture of 44*3 + 56*-1 = 76
Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results in a total score of 62842880, which happens to be the best score possible given these ingredients. If any properties had produced a negative total, it would have instead become zero, causing the whole score to multiply to zero.

Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you can make?

Your puzzle answer was 13882464.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---

Your cookie recipe becomes wildly popular! Someone asks if you can make another recipe that has exactly 500 calories per cookie (so they can use it as a meal replacement). Keep the rest of your award-winning process the same (100 teaspoons, same ingredients, same scoring system).

For example, given the ingredients above, if you had instead selected 40 teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds to 100), the total calorie count would be 40*8 + 60*3 = 500. The total score would go down, though: only 57600000, the best you can do in such trying circumstances.

Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you can make with a calorie total of 500?


 */
object Day15 extends App {
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

  case class Cookie(ins: Map[Ingredient, Int]) {
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

    override def toString: String = {
      val insStr = ins.map(e => s"${e._1.name}:${e._2}").mkString(", ")
      s"Cookie[$size][t:$total, c:$calories]($insStr)"
    }
  }
  val EmptyCookie: Cookie = Cookie(Map.empty[Ingredient, Int].withDefaultValue(0))

  def maximize(cookies: List[Cookie], n: Int, maxN: Int): List[Cookie] = {
    println(s"$n - ${cookies.size}")
    if (n < maxN) {

      val newCookies: List[Cookie] = for {
        cookie <- cookies
        in <- ingredients
      } yield cookie.add(in)

      val bestScore = newCookies.map(_.total).max

      val bestCookies = newCookies.filter(_.total == bestScore).distinct

      maximize(bestCookies, n + 1, maxN)
    }
    else {
      cookies
    }
  }

  val bestCookies = maximize(List(EmptyCookie), 0, 100)
  bestCookies foreach println

  case class CookieEntity(cookie: Cookie) extends Entity {
    override type OutEntity = CookieEntity
    override def fitness: Long = if (fitness0 < 1) 1 else fitness0
    def fitness0: Int = cookie.total - Math.abs(cookie.calories - 500)*2000000
    override def mutated(implicit rnd: Random): CookieEntity = {
      val c = cookie
        .rem(rnd.shuffle(cookie.ins.keys).head)
        .add(rnd.shuffle(ingredients).head)
      CookieEntity(c)
    }
    override def toString: String = s"CookieEntity([$fitness] $cookie)"
  }

  case object CookieFactory extends EntityFactory[CookieEntity] {
    override def random(implicit rnd: Random): CookieEntity = {
      val ins0 = ingredients zip Stream.continually(rnd.nextDouble())
      val sum = ins0.map(_._2).sum
      val ins1 = ins0.map { case (in, d) => in -> Math.round((d / sum) * 100).toInt }
      val firstAm = 100 - ins1.tail.map(_._2).sum
      val ins = (ins1.head._1, firstAm) :: ins1.tail
      val c = Cookie(ins.toMap)
      assert(c.size == 100, s"Bad sized cookie: $c")
      CookieEntity(c)
    }
    override def crossover(e1: CookieEntity, e2: CookieEntity)(implicit rnd: Random): CookieEntity = {
      val ins0 = ingredients.map { in =>
        val first = rnd.nextBoolean()
        val c = if (first) e1.cookie else e2.cookie
        val am = c.ins(in)
        (in, am)
      }
      val sum = ins0.map(_._2).sum.toDouble
      val ins1 = ins0.map { case (in, d) => in -> Math.round((d / sum) * 100).toInt }
      val firstAm = 100 - ins1.tail.map(_._2).sum
      val ins = (ins1.head._1, firstAm) :: ins1.tail
      val c = Cookie(ins.toMap)
      assert(c.size == 100, s"Bad sized cookie: $c")
      CookieEntity(c)
    }
  }

  val gen = Genetic(populationSize = 500, genCount = 100, mutationRate = 0.8, factory = CookieFactory)
  val best500Cookie = gen.optimize
  println(best500Cookie)
}
