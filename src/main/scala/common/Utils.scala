package common

import java.time.Instant
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

object Utils {
  def timed[A](f: => A): A = {
    val start = Instant.now().toEpochMilli
    val res = f
    val end = Instant.now().toEpochMilli
    val runTime = Duration.create(end - start, TimeUnit.MILLISECONDS)
    println(s"Finished in ${runTime}ms")
    res
  }
}
