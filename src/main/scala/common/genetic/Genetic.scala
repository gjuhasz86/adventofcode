package common.genetic

import scala.collection.immutable.IndexedSeq
import scala.util.Random

case class Genetic[E >: Null <: Entity](populationSize: Int,
                                        genCount: Int,
                                        mutationRate: Double,
                                        factory: EntityFactory[E])(implicit toE: E#OutEntity => E) {
  implicit val rnd = new Random()

  def optimize: E = {
    def loop(population: IndexedSeq[E], gen: Int): IndexedSeq[E] =
      if (gen >= genCount) population else {
        val next = mutate(nextGeneration(population))
        val best = next.maxBy(_.fitness)
        val avg = next.map(_.fitness).sum / populationSize
        println(s"[${best.fitness}][$avg] $best")
        loop(next, gen + 1)
      }
    loop(firstGeneration(), 0).maxBy(_.fitness)
  }

  def firstGeneration(): IndexedSeq[E] = (1 to populationSize).map(_ => factory.random)

  def nextGeneration(population: IndexedSeq[E]): IndexedSeq[E] = {
    val sumFitness = population.map(_.fitness).sum
    assert(sumFitness > 0, population.mkString("\n"))
    val rouletteWheel = roulette(population)
    val newGeneration = (1 to populationSize).map { _ =>
      val rnd1 = (rnd.nextDouble * sumFitness).toLong
      val rnd2 = (rnd.nextDouble * sumFitness).toLong
      val e1 = rouletteWheel.find(x => x._1 <= rnd1 && rnd1 <= x._2).get._3
      val e2 = rouletteWheel.find(x => x._1 <= rnd2 && rnd2 <= x._2).get._3
      val child = factory.crossover(e1, e2)
      child.mutated: E
    }
    newGeneration
  }

  def roulette(population: IndexedSeq[E]): IndexedSeq[(Long, Long, E)] =
    population.scanLeft((0L, 0L, null: E)) { case ((_, from, _), e) => (from, from + e.fitness - 1, e) }


  def mutate(population: IndexedSeq[E]): IndexedSeq[E] =
    population.map { e => if (rnd.nextDouble() < mutationRate) toE(e.mutated) else e }

}
trait EntityFactory[E] {
  def random(implicit rnd: Random): E
  def crossover(e1: E, e2: E)(implicit rnd: Random): E
}

trait Entity {
  type OutEntity <: Entity

  def fitness: Long
  def mutated(implicit rnd: Random): OutEntity

}
