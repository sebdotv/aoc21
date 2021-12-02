import aoc._

object d02 {
  sealed trait Dir
  case object Forward extends Dir
  case object Down extends Dir
  case object Up extends Dir

  case class Pos(horiz: Int, depth: Int)
  case class Cmd(dir: Dir, value: Int) {
    def applyTo(pos: Pos): Pos = dir match {
      case Forward => pos.copy(horiz = pos.horiz + value)
      case Down    => pos.copy(depth = pos.depth + value)
      case Up      => pos.copy(depth = pos.depth - value)
    }
  }

  def parseCmd(s: String): Either[String, Cmd] =
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

  def finalPos(lines: List[String]): Pos =
    lines
      .map(parseCmd)
      .map(_.unsafeGet())
      .foldLeft(Pos(0, 0))((pos, cmd) => cmd.applyTo(pos))
}
