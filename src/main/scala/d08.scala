import cats.implicits._

object d08 {
  private val segments = Vector(
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
  final case class Entry(patterns: List[String], outputs: List[String]) {
    assert(patterns.size === 10)
    assert(outputs.size === 4)
    private lazy val patternDistribution: Map[Char, Int] =
      patterns.flatten.groupBy(identity).map { case (c, v) => (c, v.size) }
    def getUniquePatternWithLength(length: Int): String = {
      val Seq(pattern) = patterns.filter(_.length === length)
      pattern
    }
    def getUniqueCharWithPatternOccurrences(count: Int): Char = {
      val Seq(c) = patternDistribution.filter(_._2 === count).keys.toList
      c
    }
  }
  object Entry {
    def parse(s: String): Entry = {
      val List(patterns, outputs) = s.split(""" \| """).map(_.split(" ").toList).toList
      Entry(patterns, outputs)
    }
  }

  class MutableSolver {
    private val candidates = Array.fill(7 * 7)(true)
    def mark(cipher: String, plain: String): Unit = {
      assert(cipher.length === plain.length)
      val cs = cipher.map(_ - 'a').toList
      val ps = plain.map(_ - 'a').toList
      for (c <- 0 until 7)
        if (cs.contains_(c)) for (p <- 0 until 7 if !ps.contains_(p)) {
          remove(c, p)
        }
        else
          for (p <- ps) {
            remove(c, p)
          }
    }
    def solution: Map[Char, Char] =
      (0 until 7).map { c =>
        getRemaining(c) match {
          case Seq(p) => ('a' + c).toChar -> ('a' + p).toChar
          case _      => throw new IllegalStateException("Solution not found")
        }
      }.toMap
    private def remove(c: Int, p: Int): Unit = {
      val offset = 7 * c + p
      if (candidates(offset)) {
        candidates(offset) = false
        getRemaining(c) match {
          case Seq(p) =>
            // remove p from others - this is actually not required to solve today's problem
            for (otherC <- 0 until 7 if otherC =!= c) {
              remove(otherC, p)
            }
          case _ => // nop
        }
      }
    }

    private def getRemaining(c: Int) = {
      (0 until 7).filter(p => candidates(7 * c + p))
    }

    def toStrLines: List[String] = {
      (0 until 7).map { c =>
        (0 until 7).map { p =>
          if (candidates(7 * c + p)) '.' else 'X'
        }.mkString
      }.toList
    }
  }

  def countEasy(input: Input): Int =
    input.map(_.outputs.count(s => s.length === 2 || s.length === 4 || s.length === 3 || s.length === 7)).sum

  def solve(entry: Entry): Int = {
    val solver = new MutableSolver()
    // unique patterns
    solver.mark(entry.getUniquePatternWithLength(2), segments(1))
    solver.mark(entry.getUniquePatternWithLength(4), segments(4))
    solver.mark(entry.getUniquePatternWithLength(3), segments(7))
//    solver.mark(entry.getUniquePatternWithLength(7), segments(8)) // useless
    // unique occurrences - standard distribution:
    // (f,9) <= unique
    // (a,8)
    // (c,8)
    // (d,7)
    // (g,7)
    // (b,6) <= unique
    // (e,4) <= unique
    solver.mark(entry.getUniqueCharWithPatternOccurrences(9).toString, "f")
    solver.mark(entry.getUniqueCharWithPatternOccurrences(6).toString, "b")
    solver.mark(entry.getUniqueCharWithPatternOccurrences(4).toString, "e")

    val solution = solver.solution

    val outputValueStr = entry.outputs.map { output =>
      val plain = output.map(solution).sorted.mkString
      segments.indexOf(plain)
    }.mkString

    outputValueStr.toInt
  }

  def part2(input: Input): Int =
    input.map(solve).sum
}
