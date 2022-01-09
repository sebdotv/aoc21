import aoc.trigo._
import cats.Eq
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object d17 {
  final case class State(pos: Coord, velocity: Vect, targetArea: TargetArea, history: List[Coord], initialVelocity: Vect, steps: Int) {
    import State._
    lazy val status: TargetArea.Status = targetArea.status(pos)

    // todo remove - attempt to compute pos from just (initialVelocity,steps)
    assert(pos.y === steps * initialVelocity.y - steps * (steps - 1) / 2, s"steps=$steps, pos=$pos, initialVelocity=$initialVelocity")

    //    println(s"steps: $steps, pos=$pos, status=$status")
    //    println(toGrid)
    def step: State =
      copy(
        pos = pos + velocity,
        velocity = Vect(
          x = velocity.x + (velocity.x match {
            case i if i > 0 => -1
            case i if i < 0 => 1
            case 0          => 0
          }),
          y = velocity.y - 1
        ),
        history = pos :: history,
        steps = steps + 1
      )
    @tailrec
    def stepN(n: Int): State =
      if (n === 0) this else step.stepN(n - 1)
    @tailrec
    def runUntilHitOrMiss: State =
      status match {
        case TargetArea.NotReached            => step.runUntilHitOrMiss
        case TargetArea.Hit | TargetArea.Miss => this
      }

    def toGridLines: List[String] = {
      val (minX, minY, maxX, maxY) = Coord.computeExtent(List(Start) ++ history ++ List(pos) ++ targetArea.coords)
      val cells = mutable.Map.empty[Coord, Char]
      targetArea.coords.foreach(cells(_) = 'T')
      (pos :: history).foreach(cells(_) = '#')
      cells(Start) = 'S'
      (maxY to minY by -1).map { y =>
        (minX to maxX).map { x =>
          cells.getOrElse(Coord(x, y), '.')
        }.mkString
      }.toList
    }
    def toGrid: String = toGridLines.mkString("\n")
  }
  object State {
    val Start: Coord = Coord.zero
    def initial(targetArea: TargetArea)(velocity: Vect): State =
      State(pos = Start, velocity = velocity, targetArea = targetArea, history = Nil, initialVelocity = velocity, steps = 0)
  }
  final case class TargetArea(minX: Int, maxX: Int, minY: Int, maxY: Int) {
    import TargetArea._
    lazy val coords: List[Coord] =
      (for (y <- minY to maxY; x <- minX to maxX) yield Coord(x, y)).toList
    def status(pos: Coord): Status =
      pos match {
        case Coord(x, y) if x >= minX && x <= maxX && y >= minY && y <= maxY => Hit
        case Coord(x, y) if x > maxX || y < minY                             => Miss
        case _                                                               => NotReached
      }
  }
  object TargetArea {
    sealed trait Status
    implicit val eqStatus: Eq[Status] = Eq.fromUniversalEquals
    case object NotReached extends Status
    case object Hit extends Status
    case object Miss extends Status
  }

  def parse(input: String): TargetArea = {
    val Re = """target area: x=([-0-9]+)\.\.([-0-9]+), y=([-0-9]+)\.\.([-0-9]+)""".r
    val Re(minX, maxX, minY, maxY) = input
    TargetArea(minX = minX.toInt, maxX = maxX.toInt, minY = minY.toInt, maxY = maxY.toInt)
  }

  private def maxHeight(targetArea: TargetArea)(velocity: Vect): Option[Int] = {
    val finalState = State.initial(targetArea)(velocity).runUntilHitOrMiss
    if (finalState.status === TargetArea.Hit) Some(finalState.history.map(_.y).max)
    else None
  }

  def part1(targetArea: TargetArea): (Vect, Int) = {
    (0 to targetArea.minX.abs)
      .flatMap { x =>
        (0 to targetArea.minY.abs).flatMap { y =>
          val v = Vect(x, y)
          maxHeight(targetArea)(v).map(h => v -> h).toList
        }
      }
      .maxBy(_._2)
  }
}
