import cats.Eq
import cats.implicits._

import scala.annotation.tailrec

object d05 {
  import Coord._

  final case class Coord(x: Int, y: Int) {
    def -(start: Coord): Vect =
      Vect(start = start, x - start.x, y - start.y)
    def +(v: Vect): Coord =
      copy(x = x + v.x, y = y + v.y)
  }
  object Coord {
    def parse(s: String): Coord =
      s.split(",").map(_.toInt) match {
        case Array(x, y) => Coord(x, y)
      }
    implicit val eqCoord: Eq[Coord] = Eq.fromUniversalEquals
  }
  final case class Vect(start: Coord, x: Int, y: Int) {
    def dir(withDiagonals: Boolean): Option[Vect] =
      if (x > 0 && y === 0) Some(Vect(start, 1, 0))
      else if (x < 0 && y === 0) Some(Vect(start, -1, 0))
      else if (x === 0 && y > 0) Some(Vect(start, 0, 1))
      else if (x === 0 && y < 0) Some(Vect(start, 0, -1))
      else if (withDiagonals) {
        if (x === y && x > 0) Some(Vect(start, 1, 1))
        else if (x === y && x < 0) Some(Vect(start, -1, -1))
        else if (x === -y && x > 0) Some(Vect(start, 1, -1))
        else if (x === -y && x < 0) Some(Vect(start, -1, 1))
        else None
      } else None
  }

  class MutableGrid(val w: Int, h: Int) {
    private val counts = Array.fill(w * h)(0)
    def addVent(start: Coord, end: Coord, withDiagonals: Boolean): Unit = {
      (end - start).dir(withDiagonals = withDiagonals) match {
        case None => // skip for now
        case Some(dir) =>
          @tailrec
          def it(p: Coord): Unit = {
            addVentPoint(p)
            if (p === end) () else it(p + dir)
          }
          it(start)
      }
    }
    def addVentPoint(p: Coord): Unit =
      counts(p.x + p.y * w) += 1
    def toStrLines: List[String] =
      (0 until h).map { y =>
        (0 until w).map { x =>
          counts(x + y * w) match {
            case 0           => '.'
            case n if n <= 9 => ('0' + n).toChar
          }
        }.mkString
      }.toList
    def countDangerous: Int =
      counts.count(_ >= 2)
  }

  def parse(input: List[String], withDiagonals: Boolean): MutableGrid = {
    val vents = input.map(_.split(" -> ").map(Coord.parse) match {
      case Array(start, end) => start -> end
    })
    val (starts, ends) = vents.unzip
    val allCoords = starts ++ ends
    val grid = new MutableGrid(w = allCoords.map(_.x).max + 1, h = allCoords.map(_.y).max + 1)
    vents.foreach { case (start, end) =>
      grid.addVent(start, end, withDiagonals = withDiagonals)
    }
    grid
  }
}
