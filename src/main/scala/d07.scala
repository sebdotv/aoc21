import aoc._

object d07 {
  final case class Target(pos: Int, fuel: Int)
  type FuelFn = (Int, Int) => Int
  final case class Puzzle(crabs: List[Int], fuelFn: FuelFn) {
    lazy val min = crabs.minOption.unsafeGet()
    lazy val max = crabs.maxOption.unsafeGet()
    def fuelNeededTo(p: Int): Int =
      crabs.map(c => fuelFn(c, p)).sum
    def bruteForceBest: Target =
      (min to max).map(p => Target(pos = p, fuel = fuelNeededTo(p))).minByOption(_.fuel).unsafeGet()
  }

  val part1FuelFn: FuelFn = (a, b) => Math.abs(b - a)

  def center(positions: List[Int]): Int =
    positions.sum / positions.size
}
