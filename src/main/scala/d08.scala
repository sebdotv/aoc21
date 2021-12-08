import aoc._
import cats.implicits._

import scala.annotation.tailrec

object d08 {
  val segments = Vector(
    "abcefg", // 0
    "cf", // 1
    "acdeg", // 2
    "acdfg", // 3
    "bcdf", // 4
    "abdfg", // 5
    "abdefg", // 6
    "acf", // 7
    "abcdefg", // 8
    "abcdfg" // 9
  )

  type Input = List[Entry]
  case class Entry(patterns: List[String], outputs: List[String]) {
    assert(patterns.size === 10)
    assert(outputs.size === 4)
  }
  object Entry {
    def parse(s: String): Entry = {
      val Array(patterns, outputs) = s.split(""" \| """).map(_.split(" ").toList)
      Entry(patterns, outputs)
    }
  }

  def countEasy(input: Input): Int =
    input.map(_.outputs.count(s => s.length === 2 || s.length === 4 || s.length === 3 || s.length === 7)).sum
}
