package aoc2018

import java.lang.Math.min

import scala.collection.Set
import scala.io.Source

object Day07 {
  val testIn =
    """Step C must be finished before step A can begin.
      |Step C must be finished before step F can begin.
      |Step A must be finished before step B can begin.
      |Step A must be finished before step D can begin.
      |Step B must be finished before step E can begin.
      |Step D must be finished before step E can begin.
      |Step F must be finished before step E can begin.""".stripMargin.lines.map(parseLine).toList

  def input = parse("src/main/resources/aoc2018/day07.lorant.txt")

  def parse(file: String) =
    Source.fromFile(file).getLines.map(parseLine).toList

  def parseLine(str: String) = (str(36), str(5))

  type TaskId = Char
  type Dep = (TaskId, Set[TaskId])

  implicit class DepOps(val d: Dep) extends AnyVal {
    def id = d._1
    def deps = d._2
  }

  def main(args: Array[String]): Unit = {
    val res1 = ex1(input)
    println(s"RES1: $res1")
    val res2 = ex2(input)
    println(s"RES2: $res2")
  }

  // PART1 ------------------

  def ex1(in: List[(TaskId, TaskId)]) = {
    val allTasks: List[Dep] =
      (in.map(_._2).map(_ -> Set[Char]()).toMap ++
        in.groupBy(_._1).mapValues(_.map(_._2).toSet)).toList.sortBy(_._1)
    process(allTasks)
  }


  def process(tasks: List[Dep]): String = process(Nil, tasks).reverse.mkString
  def process(order: List[Char], tasks: List[Dep]): List[Char] =
    tasks match {
      case Nil => order
      case _ =>
        val next = tasks.collectFirst { case (id, deps) if deps.isEmpty => id }.get
        val newTasks = tasks.collect { case (id, deps) if id != next => (id, deps - next) }
        process(next :: order, newTasks)
    }


  // PART2 ------------------

  def time(t: TaskId) = t - 'A' + 61

  def ex2(in: List[(TaskId, TaskId)]) = {
    val allTasks: List[Dep] =
      (in.map(_._2).map(_ -> Set[Char]()).toMap ++
        in.groupBy(_._1).mapValues(_.map(_._2).toSet)).toList.sortBy(_._1)

    parProcess(allTasks, 5)
  }

  def parProcess(tasks: List[Dep], wCount: Int): Int = {
    def loop(s: Scheduler): Scheduler =
      if (s.isDone)
        s
      else
        loop(s.step)

    val taskList = tasks.map { case (id, deps) => Task.of(id, deps) }
    val depMan = new DepManager(taskList, Set())
    val sched = new Scheduler(0, List.fill(wCount)(Worker(None)), depMan)
    loop(sched).time
  }

  object Task {def of(id: TaskId, deps: Set[TaskId]) = Task(id, time(id), deps) }
  case class Task(id: TaskId, size: Int, deps: Set[TaskId]) {
    def step = this.copy(size = size - 1)
    def isDone = size <= 0
    def removeDep(task: Task) = this.copy(deps = deps - task.id)
    def canStart = deps.isEmpty
  }

  class DepManager(val tasks: List[Task], inProgress: Set[TaskId]) {
    lazy val next: Option[Task] =
      tasks.collectFirst { case task if !inProgress.contains(task.id) && task.canStart => task }
    def setInProgress(task: Option[Task]): DepManager =
      task.map(setInProgress).getOrElse(this)
    def setInProgress(task: Task): DepManager =
      new DepManager(tasks, inProgress + task.id)
    def markDone(task: Task): DepManager =
      new DepManager(tasks.filter(_.id != task.id).map(_.removeDep(task)), inProgress)
    def markDone(completed: List[Task]): DepManager =
      DepManager.markDone(this, completed)
  }
  object DepManager {
    private def markDone(dm: DepManager, completed: List[Task]): DepManager =
      completed match {
        case Nil => dm
        case h :: t => markDone(dm.markDone(h), t)
      }

  }

  class Scheduler(val time: Int, workers: List[Worker], depMan: DepManager) {
    def step: Scheduler = {
      val (newWorkers1, newDepMan1) =
        workers.foldLeft((List[Worker](), depMan)) {
          case ((acc, dm), Worker(None)) =>
            (acc :+ Worker(dm.next), dm.setInProgress(dm.next))
          case ((acc, dm), w) => (acc :+ w, dm)
        }
      print(s"$time\t")
      println(newWorkers1.map(_.task.map(_.id).getOrElse('.')).mkString("\t"))
      val newWorkers = newWorkers1.map(_.step)
      val newDepMan = newDepMan1.markDone(newWorkers1.flatMap(_.getTaskIfDone))
      new Scheduler(time + 1, newWorkers, newDepMan)
    }
    def isDone = depMan.tasks.isEmpty
  }

  case class Worker(task: Option[Task]) {
    def step: Worker = this.copy(task.map(_.step).filter(!_.isDone))
    def getTaskIfDone = task.map(_.step).filter(_.isDone)
  }

}
