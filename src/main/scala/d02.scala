import aoc._

object d02 {
  sealed trait Dir
  case object Forward extends Dir
  case object Down extends Dir
  case object Up extends Dir

  final case class State(horiz: Int, depth: Int, aim: Int) {
    def pos: (Int, Int) = (horiz, depth)
  }
  object State {
    def zero: State = State(0, 0, 0)
  }
  final case class Cmd(dir: Dir, x: Int)
  object Cmd {
    def parse(s: String): Either[String, Cmd] =
      s.split(" ") match {
        case Array(d, v) =>
          Right(
            Cmd(
              d match {
                case "forward" => Forward
                case "down"    => Down
                case "up"      => Up
              },
              v.toInt
            )
          )
        case _ => Left("illegal command")
      }
  }

  type Interpreter = (State, Cmd) => State
  val interpreter1: Interpreter = { case (state, Cmd(dir, x)) =>
    dir match {
      case Forward => state.copy(horiz = state.horiz + x)
      case Down    => state.copy(depth = state.depth + x)
      case Up      => state.copy(depth = state.depth - x)
    }
  }
  val interpreter2: Interpreter = { case (state, Cmd(dir, x)) =>
    dir match {
      case Forward => state.copy(horiz = state.horiz + x, depth = state.depth + state.aim * x)
      case Down    => state.copy(aim = state.aim + x)
      case Up      => state.copy(aim = state.aim - x)
    }
  }

  def finalState(lines: List[String], interpreter: Interpreter): State =
    lines
      .map(Cmd.parse)
      .map(_.unsafeGet())
      .foldLeft(State.zero)(interpreter)
}
