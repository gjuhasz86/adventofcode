package aoc2015

import spray.json._
import DefaultJsonProtocol._

import scala.io.Source

object Day12 extends App {

    val input = Source.fromFile("src/main/scala/aoc2015/day12.txt").mkString
//  val input =
//    """[1,{"c":"red","b":2},3]"""
  val inputJson: JsValue = input.parseJson

  def sum(json: JsValue): Int = json match {
    case JsArray(arr) => arr.map(sum).sum
    case JsNumber(num) => num.toInt
    case JsObject(obj) =>
      if (obj.values.toSet.contains("red".toJson)) 0 else obj.values.map(sum).sum
    case _ => 0
  }

  val res = sum(inputJson)
  println(res)
}
