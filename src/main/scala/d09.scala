import cats.implicits._

import aoc._

object d09 {
  final case class Coord(x: Int, y: Int)
  final case class Heightmap(w: Int, h: Int, data: Vector[Int]) {
    assert(data.length === w * h)

    def get(c: Coord): Int =
      data(c.x + c.y * w)

    def adjacentLocations(c: Coord): List[Coord] =
      List(
        if (c.y > 0) List(c.copy(y = c.y - 1)) else Nil,
        if (c.y < h - 1) List(c.copy(y = c.y + 1)) else Nil,
        if (c.x > 0) List(c.copy(x = c.x - 1)) else Nil,
        if (c.x < w - 1) List(c.copy(x = c.x + 1)) else Nil
      ).flatten

    lazy val lowPoints: List[Coord] = {
      val coords = for (y <- 0 until h; x <- 0 until w) yield Coord(x, y)
      coords.filter(c => adjacentLocations(c).forall(get(c) < get(_))).toList
    }
  }

  def parse(input: List[String]): Heightmap = {
    val w = input.headOption.unsafeGet().length
    val h = input.size
    assert(input.forall(_.length === w))
    Heightmap(w = w, h = h, input.flatMap(_.map(_ - '0')).toVector)
  }

  def totalRiskLevel(heightmap: Heightmap): Int =
    heightmap.lowPoints.map(heightmap.get(_) + 1).sum

  private def computeBasins(hm: Heightmap): Vector[Option[Coord]] = {
    sealed trait Cell
    case object NotComputed extends Cell
    case object NoBasin extends Cell
    final case class Basin(c: Coord) extends Cell

    val cells: Array[Cell] = Array.fill(hm.w * hm.h)(NotComputed)

    def computeCell(c: Coord): Cell = {
      val ofs = c.x + c.y * hm.w
      cells(ofs) match {
        case NotComputed =>
          val cell = hm.get(c) match {
            case 9 => NoBasin
            case _ =>
              hm.adjacentLocations(c).minByOption(hm.get).filter(hm.get(_) < hm.get(c)) match {
                case Some(downward) => computeCell(downward)
                case None           => Basin(c)
              }
          }
          cells(ofs) = cell
          cell
        case computed => computed
      }
    }

    for (y <- 0 until hm.h; x <- 0 until hm.w) {
      computeCell(Coord(x, y))
    }

    cells.map {
      case NotComputed => throw new IllegalStateException("Not computed")
      case NoBasin     => None
      case Basin(c)    => Some(c)
    }.toVector
  }

  def sortedBasinSizes(hm: Heightmap): List[Int] = {
    val basins = computeBasins(hm)
    basins.groupBy(identity).collect { case (Some(_), list) => list.length }.toList.sorted
  }

  def part2(hm: Heightmap): Int =
    sortedBasinSizes(hm).takeRight(3).product
}
