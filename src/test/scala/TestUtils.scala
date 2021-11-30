import cats.effect.unsafe.implicits.global

object TestUtils {
  def load(filename: String): List[String] = aoc.load(filename).unsafeRunSync()
  def loadLine(filename: String): String = aoc.loadLine(filename).unsafeRunSync()

  implicit class StringSplitLinesImprovements(s: String) {
    def splitLines: List[String] = s.trim.split("\n").toList
  }
}
