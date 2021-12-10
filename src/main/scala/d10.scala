import cats.implicits._

import scala.annotation.tailrec

object d10 {
  private val Starts = "([{<".toList
  private val Ends = ")]}>".toList

  sealed trait ParseError
  object ParseError {
    case class Corrupted(expected: Char, found: Char) extends ParseError
    case object Incomplete extends ParseError
    case object EndWithoutStart extends ParseError
  }
  import ParseError._

  implicit class ParseResultOps(self: Either[ParseError, Unit]) {
    def isCorrupted: Boolean = self match {
      case Left(Corrupted(_, _)) => true
      case _                     => false
    }
    def isIncomplete: Boolean = self match {
      case Left(Incomplete) => true
      case _                => false
    }
  }

  def parse(line: String): Either[ParseError, Unit] = {
    @tailrec
    def it(remain: List[Char], acc: List[Char]): Either[ParseError, Unit] =
      remain match {
        case Nil =>
          if (acc.isEmpty) Right(())
          else Left(ParseError.Incomplete)
        case h :: t =>
          h match {
            case start if Starts.contains_(start) =>
              it(t, start :: acc)
            case end if Ends.contains_(end) =>
              acc match {
                case Nil =>
                  Left(ParseError.EndWithoutStart)
                case top :: rest =>
                  val i = Starts.indexOf(top)
                  if (Ends.indexOf(end) === i) it(t, rest)
                  else Left(ParseError.Corrupted(expected = Ends(i), found = end))
              }
          }
      }
    it(line.toList, Nil)
  }

  def score(line: String): Int =
    parse(line) match {
      case Left(Corrupted(_, ')')) => 3
      case Left(Corrupted(_, ']')) => 57
      case Left(Corrupted(_, '}')) => 1197
      case Left(Corrupted(_, '>')) => 25137
      case _                       => 0
    }

  def score(input: List[String]): Int =
    input.map(score).sum
}
