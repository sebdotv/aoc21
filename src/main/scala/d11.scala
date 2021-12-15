import aoc._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object d11 {
  final case class Grid(h: Int, w: Int, energyLevels: Vector[Int], totalFlashes: Int, steps: Int) {
    assert(energyLevels.size === h * w)

    def nSteps(n: Int): Grid =
      (1 to n).foldLeft(this)((curr, _) => curr.step)

    def step: Grid = {
      val data = energyLevels.toArray
      val flashed = mutable.Set.empty[(Int, Int)]

      def adjacent(x: Int, y: Int) = for (b <- y - 1 to y + 1; a <- x - 1 to x + 1 if (a, b) =!= (x, y)) yield (a, b)
      def get_(x: Int, y: Int): Int = data(x + y * w)
      def set_(x: Int, y: Int)(value: Int): Unit = data(x + y * w) = value
      def inc_(x: Int, y: Int): Unit = {
        val value = get_(x, y) + 1
        set_(x, y)(value)
        if (value > 9) flash_(x, y)
      }
      def flash_(x: Int, y: Int): Unit = {
        if (!flashed.contains((x, y))) {
          flashed.add((x, y))
          adjacent(x, y).filter { case (x, y) => x >= 0 && x < w && y >= 0 && y < h }.foreach(inc)
        }
      }
      def set = (set_ _).tupled
      def inc = (inc_ _).tupled

      def coords = for (y <- 0 until h; x <- 0 until w) yield (x, y)

      coords.foreach(inc)

      flashed.foreach(set(_)(0))

      copy(energyLevels = data.toVector, totalFlashes = totalFlashes + flashed.size, steps = steps + 1)
    }

    override def toString: String =
      energyLevels.map(_ + '0').map(_.toChar).mkString.grouped(w).mkString("\n")
  }

  def parseGrid(input: List[String]): Grid = {
    val w = input.headOption.unsafeGet().length
    assert(input.forall(_.length === w))
    Grid(w, input.size, input.flatten.map(_ - '0').toVector, totalFlashes = 0, steps = 0)
  }

  @tailrec
  def firstSimultaneousFlashStep(input: Grid): Int = {
    val next = input.step
    if (next.energyLevels.forall(_ === 0)) next.steps
    else firstSimultaneousFlashStep(next)
  }
}
