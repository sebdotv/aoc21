import aoc._
import aoc.trigo._
import cats.Eval
import cats.implicits._

object d09 {
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

    def coords: Vector[Coord] =
      (for (y <- 0 until h; x <- 0 until w) yield Coord(x, y)).toVector

    lazy val lowPoints: List[Coord] = {
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
    object MemoizedSolver {
      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      private var computations: Int = 0
      private val evals: Vector[Eval[Option[Coord]]] = hm.coords.map(computeCell)

      private def computeCell(c: Coord): Eval[Option[Coord]] =
        Eval.defer {
          computations += 1
          hm.get(c) match {
            case 9 => Eval.always(None)
            case _ =>
              hm.adjacentLocations(c).minByOption(hm.get).filter(hm.get(_) < hm.get(c)) match {
                case Some(downward) => evals(downward.x + downward.y * hm.w)
                case None           => Eval.always(Some(c))
              }
          }
        }.memoize

      def basins: Vector[Option[Coord]] = {
        val result = evals.map(_.value)
        assert(computations === hm.w * hm.h)
        result
      }
    }
    MemoizedSolver.basins
  }

  def sortedBasinSizes(hm: Heightmap): List[Int] = {
    val basins = computeBasins(hm)
    basins.groupBy(identity).collect { case (Some(_), list) => list.length }.toList.sorted
  }

  def part2(hm: Heightmap): Int =
    sortedBasinSizes(hm).takeRight(3).product
}
