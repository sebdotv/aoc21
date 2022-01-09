package aoc.trigo

final case class Vect(x: Int, y: Int) {
  def norm: Double = Math.sqrt(x * y)
  def manhattanDistance: Int = Math.abs(x) + Math.abs(y)
}
object Vect {
  private lazy val Re = """(-?\d+),(-?\d+)""".r
  def parseUnsafe(s: String): Vect = {
    val Re(x, y) = s
    Vect(x.toInt, y.toInt)
  }
}
