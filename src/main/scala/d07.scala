import aoc._
import cats.implicits._

import scala.annotation.tailrec

object d07 {
  final case class Target(pos: Int, fuel: Int)
  type FuelFn = (Int, Int) => Int
  final case class Puzzle(crabs: List[Int], fuelFn: FuelFn) {
    private lazy val min = crabs.minOption.unsafeGet()
    private lazy val max = crabs.maxOption.unsafeGet()
    def fuelNeededTo(p: Int): Int =
      crabs.map(c => fuelFn(c, p)).sum
    def bruteForceBest: Target =
      (min to max).map(p => Target(pos = p, fuel = fuelNeededTo(p))).minByOption(_.fuel).unsafeGet()
  }

  val part1FuelFn: FuelFn = (a, b) => Math.abs(b - a)
  private val part2FuelFnSlow: FuelFn = (a, b) =>
    if (a === b) 0
    else {
      val sign = (b - a) / Math.abs(b - a)
      @tailrec
      def it(curr: Int, acc: Int, step: Int): Int =
        if (curr === b) acc
        else it(curr + sign, acc + step, step + 1)
      it(a, 0, 1)
    }
  private val part2FuelFnFast: FuelFn = (a, b) => {
    val dist = Math.abs(b - a)
    (dist + 1) * dist / 2
  }
  val part2FuelFn: FuelFn = part2FuelFnFast

  def center(positions: List[Int]): Int =
    positions.sum / positions.size
}
