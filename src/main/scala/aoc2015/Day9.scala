package aoc2015

import scala.collection.immutable.Seq
import scala.io.Source

object Day9 extends App {
  val input = Source.fromFile("src/main/scala/aoc2015/day9.txt").getLines().toList
  println(input)

  case class Route(from: String, to: String, dist: Int)

  def parse(str: String) = {
    val p = str.split(" ")
    Route(p(0), p(2), p(4).toInt)
  }

  val singleRoutes = input map parse
  singleRoutes foreach println
  println()
  val routes = singleRoutes.flatMap(r => r :: Route(r.to, r.from, r.dist) :: Nil)
  routes foreach println


  val cities = routes.flatMap(r => r.from :: r.to :: Nil).distinct.sorted
  println(cities)

  def findShortest(paths: List[(Int, List[Route])]): List[(Int, List[Route])] = {
    println()
    println("Paths:")
    paths foreach println
    println()
    paths match {
      case Nil => Nil
      case (len, soFar) :: rest =>
        println()
        println(s"[$len] $soFar")
        val visited = (soFar.map(_.from) ++ soFar.map(_.to)).distinct
        val unvisited = cities diff visited
        println(s"unvisited: $unvisited")
        if (unvisited.isEmpty) {
          paths
        } else {
          val nextPaths = routes
            .filter(_.from == soFar.last.to)
            .filter(r => unvisited.contains(r.to))
            .map(r => (len + r.dist, soFar :+ r))
          println("Added:")
          nextPaths foreach println
          println()
          val newPaths = (rest ::: nextPaths).distinct.sortBy(_._1)
          findShortest(newPaths)
        }
    }
  }

  //    val allPaths = findShortest(routes.map(r => (r.dist, List(r))).sortBy(_._1))
  //    println(allPaths)
  //    val shortest = allPaths.head
  //    println(shortest)

  def findLongest(paths: List[(Int, List[Route])]): List[(Int, List[Route])] = {
    println()
    println("Paths:")
    paths.take(5) foreach println
    println()
    paths match {
      case Nil => Nil
      case (len, soFar) :: rest =>
        println()
        println(s"[$len] $soFar")
        val visited = (soFar.map(_.from) ++ soFar.map(_.to)).distinct
        val unvisited = cities diff visited
        println(s"unvisited: $unvisited")
        if (unvisited.isEmpty) {
          paths
        } else {
          val possMax = singleRoutes.filterNot(r => visited.contains(r.from) || visited.contains(r.to)).map(_.dist).sum
          println(s"PossMax: $possMax")
          val nextPaths = routes
            .filter(_.from == soFar.last.to)
            .filter(r => unvisited.contains(r.to))
            .map(r => (soFar.map(_.dist).sum + r.dist + possMax, soFar :+ r))
          println("Added:")
          nextPaths foreach println
          println()
          val newPaths = (rest ::: nextPaths).distinct.sortBy(_._1 * -1)
          findLongest(newPaths)
        }
    }
  }

  val allPaths2 = findLongest(routes.map(r => (singleRoutes.map(_.dist).sum, List(r))).sortBy(_._1 * -1))
  println(allPaths2)
  val longest = allPaths2.head
  println(longest)
  println(longest._2.map(_.dist).sum)

}
