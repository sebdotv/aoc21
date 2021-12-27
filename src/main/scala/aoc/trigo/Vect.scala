package aoc.trigo

final case class Vect(x: Int, y: Int) {
  def norm: Double = Math.sqrt(x * y)
  def manhattanDistance: Int = Math.abs(x) + Math.abs(y)
}
