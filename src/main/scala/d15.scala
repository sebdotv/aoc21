import aoc.Graph._
import aoc._
import aoc.trigo.Coord
import cats.Eval
import cats.implicits._

object d15 {
  final case class CaveMap(w: Int, h: Int, riskLevels: Vector[Int]) {
    assert(riskLevels.size === w * h)

    lazy val start: Coord = Coord(0, 0)
    lazy val end: Coord = Coord(w - 1, h - 1)

    def coords: List[Coord] =
      (for (y <- 0 until h; x <- 0 until w) yield Coord(x, y)).toList

    def insideBounds(c: Coord): Boolean =
      c.x >= 0 && c.x < w && c.y >= 0 && c.y < h

    def neighbors(c: Coord): List[Coord] =
      List(c.copy(x = c.x + 1), c.copy(y = c.y + 1)).filter(insideBounds)

    def riskLevel(c: Coord): Int =
      riskLevels(c.x + c.y * w)

    def toStrLines: List[String] =
      (0 until h).map { y =>
        (0 until w).map(x => riskLevel(Coord(x, y))).mkString
      }.toList
    override def toString: String = toStrLines.mkString("\n")
  }

  def parse(input: List[String]): CaveMap = {
    val w = input.headOption.unsafeGet().length
    assert(input.forall(_.length === w))
    CaveMap(w, input.size, input.flatMap(_.map(_ - '0')).toVector)
  }

  def solveSlow(input: CaveMap): Int = {
    val paths = dfs(input.neighbors)(VisitedFunction.fromEq)(input.start)
    def totalRisk(path: Path[Coord]) = path.toList.drop(1).map(input.riskLevel).sum
    paths.map(p => p -> totalRisk(p)).minBy(_._2)._2
  }

  def solve(input: CaveMap): Int =
    rawSolve(input)._1

  type R = (Int, List[Coord])
  def rawSolve(input: CaveMap): (Int, List[Coord]) = {
    object CachedSolver {
      private var computations = 0
      val evalMap: Map[Coord, Eval[R]] = input.coords.map(c => c -> lowestRiskTo(c)).toMap
      def lowestRiskTo(c: Coord): Eval[R] = Eval.defer {
        computations += 1
        if (c === input.start) Eval.now((0, List(c)))
        else
          List(c.copy(x = c.x - 1), c.copy(y = c.y - 1))
            .filter(input.insideBounds)
            .traverse(evalMap)
            .map(_.minBy(_._1))
            .map { case (min, acc) =>
              (min + input.riskLevel(c), c :: acc)
            }
      }.memoize
      def solve: R = {
        val result = evalMap(input.end).value
        assert(CachedSolver.computations === input.w * input.h)
        result
      }
    }
    val (solution, steps) = CachedSolver.solve
    (solution, steps.reverse)
  }

  def growForPart2(input: CaveMap): CaveMap =
    CaveMap(
      w = input.w * 5,
      h = input.h * 5,
      riskLevels = {
        (for {
          y <- 0 until input.h * 5
          x <- 0 until input.w * 5
        } yield {
          val base = input.riskLevel(Coord(x % input.w, y % input.h))
          (base + x / input.w + y / input.h - 1) % 9 + 1
        }).toVector
      }
    )
}
