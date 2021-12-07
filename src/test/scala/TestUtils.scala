import cats.effect.unsafe.implicits.global

import java.util.concurrent.TimeUnit

object TestUtils {
  def unsafeLoad(filename: String): List[String] = aoc.load(filename).unsafeRunSync()
  def unsafeLoadLine(filename: String): String = aoc.loadLine(filename).unsafeRunSync()

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + TimeUnit.MILLISECONDS.convert(t1 - t0, TimeUnit.NANOSECONDS) + "ms")
    result
  }

  implicit class StringSplitLinesImprovements(s: String) {
    def splitLines: List[String] = s.trim.split("\n").toList
  }
}
