import cats.effect.unsafe.implicits.global

object TestUtils {
  def unsafeLoad(filename: String): List[String] = aoc.load(filename).unsafeRunSync()
  def unsafeLoadLine(filename: String): String = aoc.loadLine(filename).unsafeRunSync()

  implicit class StringSplitLinesImprovements(s: String) {
    def splitLines: List[String] = s.trim.split("\n").toList
  }
}
