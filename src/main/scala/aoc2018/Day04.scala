package aoc2018

import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, TemporalUnit}

import scala.collection.immutable.BitSet
import scala.io.Source

object Day04 {

  def testIn =
    """[1518-11-01 00:00] Guard #10 begins shift
      |[1518-11-01 00:05] falls asleep
      |[1518-11-01 00:25] wakes up
      |[1518-11-01 00:30] falls asleep
      |[1518-11-01 00:55] wakes up
      |[1518-11-01 23:58] Guard #99 begins shift
      |[1518-11-02 00:40] falls asleep
      |[1518-11-02 00:50] wakes up
      |[1518-11-03 00:05] Guard #10 begins shift
      |[1518-11-03 00:24] falls asleep
      |[1518-11-03 00:29] wakes up
      |[1518-11-04 00:02] Guard #99 begins shift
      |[1518-11-04 00:36] falls asleep
      |[1518-11-04 00:46] wakes up
      |[1518-11-05 00:03] Guard #99 begins shift
      |[1518-11-05 00:45] falls asleep
      |[1518-11-05 00:55] wakes up"""
      .stripMargin
      .lines
      .map(parseLine).toList

  def main(args: Array[String]): Unit = {
    val input = parse("src/main/resources/aoc2018/day04.txt")

    //    val res1 = ex1(input)
//    println(s"RES1: $res1")
val res2 = ex2(input)
    println(s"RES1: $res2")
  }

  def ex1(in: List[Schedule]) = {
    val data = getData(in)

    val sleepyGuard =
      data
        .groupBy(_.id)
        .mapValues(countAsleepX(0))
        .maxBy(_._2)
        ._1

    println(sleepyGuard)

    val (minute, _) =
      chooseMinute(
        data
          .filter(_.id == sleepyGuard)
          .groupBy(_.time.toLocalDate)
          .mapValues(countAsleep)
          .values
      )
    println(minute)

    sleepyGuard * minute
  }

  def ex2(in: List[Schedule]) = {
    val data = getData(in)

    val (gid, (min, _)) =
      data
        .groupBy(x => (x.id, x.time.toLocalDate))
        .mapValues(countAsleep)
        .groupBy(_._1._1)
        .mapValues(_.values)
        .mapValues(chooseMinute)
        .maxBy(_._2._2)

    gid * min
  }

  def getData(in: List[Schedule]) =
    in
      .sortBy(_.time.toEpochSecond(ZoneOffset.UTC))
      .foldLeft(List[Schedule]()) { (acc, s) =>
        if (s.id == 0) acc :+ s.copy(id = acc.last.id) else acc :+ s
      }
      .map {
        case sched@Schedule(_, _, ShiftBegin) =>
          val trunkTime = sched.time.truncatedTo(ChronoUnit.DAYS)
          if (ChronoUnit.HOURS.between(trunkTime, sched.time) < 12)
            sched.copy(time = trunkTime, action = WakesUp)
          else
            sched.copy(time = trunkTime.plusDays(1), action = WakesUp)
        case sched => sched
      }


  def chooseMinute(sleeps: Iterable[BitSet]) =
    (0 until 60)
      .map { i => (i, sleeps.count(_.contains(i))) }
      .maxBy(_._2)


  def countAsleep(scheds: List[Schedule]): BitSet = countAsleep(BitSet())(scheds)
  def countAsleep(acc: BitSet)(scheds: List[Schedule]): BitSet = scheds match {
    case s0 :: (rest@(s1 :: _)) if s0.action == FallsAsleep =>
      val bits = s0.time.getMinute until s1.time.getMinute
      countAsleep(acc ++ bits)(rest)
    case s :: Nil if s.action == FallsAsleep =>
      val bits = s.time.getMinute until 60
      acc ++ bits
    case s :: rest => countAsleep(acc)(rest)
    case Nil => acc
  }


  def countAsleepX(acc: Int)(scheds: List[Schedule]): Int = scheds match {
    case s0 :: (rest@(s1 :: _)) if s0.action == FallsAsleep =>
      countAsleepX(acc + ChronoUnit.MINUTES.between(s0.time, s1.time).toInt)(rest)
    case s :: Nil if s.action == FallsAsleep =>
      acc + ChronoUnit.MINUTES.between(s.time, s.time.withHour(1).withMinute(0)).toInt
    case s :: rest => countAsleepX(acc)(rest)
    case Nil => acc
  }

  def parse(file: String) =
    Source.fromFile(file).getLines.map(parseLine).toList

  val LineRegEx = raw"\[(.+)\] (Guard #([0-9]*) )?(.*)".r

  val dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parseLine(str: String): Schedule = str match {
    case LineRegEx(time, _, id, "begins shift") => Schedule(id.toInt, LocalDateTime.parse(time, dtf), ShiftBegin)
    case LineRegEx(time, _, _, "falls asleep") => Schedule(0, LocalDateTime.parse(time, dtf), FallsAsleep)
    case LineRegEx(time, _, _, "wakes up") => Schedule(0, LocalDateTime.parse(time, dtf), WakesUp)
  }

  case class Schedule(id: Int, time: LocalDateTime, action: Action)
  sealed trait Action
  case object ShiftBegin extends Action
  case object WakesUp extends Action
  case object FallsAsleep extends Action

}
