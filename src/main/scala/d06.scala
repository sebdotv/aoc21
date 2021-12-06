import cats.implicits._

import scala.annotation.tailrec

object d06 {
  sealed trait Sim {
    def indivCount: Long
    def next: Sim
  }
  implicit class SimOps(sim: Sim) {
    def skipDays(days: Int): Sim = {
      @tailrec
      def it(remain: Int, curr: Sim): Sim =
        remain match {
          case 0 => curr
          case i => it(i - 1, curr.next)
        }
      it(days, sim)
    }
  }

  final case class SlowSim(indivs: Vector[Int]) extends Sim {
    import SlowSim._
    override def indivCount: Long = indivs.size.toLong
    def next: SlowSim =
      copy(indivs = evolve(indivs))
  }
  object SlowSim {
    def evolve(indivs: Vector[Int]): Vector[Int] = {
      val (updated, newIndivs) = indivs.map {
        case 0 => (6, List(8))
        case n => (n - 1, Nil)
      }.unzip
      updated ++ newIndivs.flatten
    }
  }

  final case class FastSim(indivsByAge: Array[Long]) extends Sim {
    assert(indivsByAge.length === 9)
    def indivCount: Long = indivsByAge.sum
    def next: FastSim = {
      copy(indivsByAge =
        Array(
          indivsByAge(1), // 0
          indivsByAge(2), // 1
          indivsByAge(3), // 2
          indivsByAge(4), // 3
          indivsByAge(5), // 4
          indivsByAge(6), // 5
          indivsByAge(7) + indivsByAge(0), // 6
          indivsByAge(8), // 7
          indivsByAge(0) // 8
        )
      )
    }
  }
  object FastSim {
    def fromIndivs(indivs: Seq[Int]): FastSim = {
      val counts = for (i <- 0 to 8) yield indivs.count(_ === i).toLong
      FastSim(counts.toArray)
    }
  }
}
