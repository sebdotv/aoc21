package aoc.trigo

final case class Vect(start: Coord, x: Int, y: Int) {
  def norm: Double = Math.sqrt(x * y)
}
