package aoc.trigo

import cats.Eq

import scala.annotation.tailrec

final case class Coord(x: Int, y: Int) {
  def -(start: Coord): Vect =
    Vect(start = start, x - start.x, y - start.y)
  def +(v: Vect): Coord =
    Coord(x = x + v.x, y = y + v.y)
}
object Coord {
  implicit val eqCoord: Eq[Coord] = Eq.fromUniversalEquals

  def parse(s: String): Coord =
    s.split(",").map(_.toInt).toList match {
      case List(x, y) => Coord(x, y)
      case other      => throw new IllegalArgumentException(s"Invalid coord: $other}")
    }

  @tailrec
  def computeExtent(coords: List[Coord], minX: Int = 0, minY: Int = 0, maxX: Int = 0, maxY: Int = 0): (Int, Int, Int, Int) =
    coords match {
      case Nil    => (minX, minY, maxX, maxY)
      case h :: t => computeExtent(t, minX = math.min(minX, h.x), minY = math.min(minY, h.y), maxX = math.max(maxX, h.x), maxY = math.max(maxY, h.y))
    }
}
