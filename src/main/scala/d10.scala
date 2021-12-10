import cats.implicits._

import scala.annotation.tailrec

object d10 {
  private val Starts = "([{<".toList
  private val Ends = ")]}>".toList

  sealed trait ParseError
  final case class Corrupted(expected: Char, found: Char) extends ParseError
  final case class Incomplete(missing: List[Char]) extends ParseError

  implicit class ParseResultOps(self: Either[ParseError, Unit]) {
    def isCorrupted: Boolean = self match {
      case Left(Corrupted(_, _)) => true
      case _                     => false
    }
    def isIncomplete: Boolean = self match {
      case Left(Incomplete(_)) => true
      case _                   => false
    }
  }

  def parse(line: String): Either[ParseError, Unit] = {
    @tailrec
    def it(remain: List[Char], acc: List[Char]): Either[ParseError, Unit] =
      remain match {
        case Nil =>
          acc match {
            case Nil =>
              Right(())
            case starts =>
              Left(Incomplete(missing = starts.map(start => Ends(Starts.indexOf(start)))))
          }
        case h :: t =>
          h match {
            case start if Starts.contains_(start) =>
              it(t, start :: acc)
            case end if Ends.contains_(end) =>
              acc match {
                case top :: rest =>
                  val i = Starts.indexOf(top)
                  if (Ends.indexOf(end) === i) it(t, rest)
                  else Left(Corrupted(expected = Ends(i), found = end))
                case Nil => throw new IllegalArgumentException("empty acc")
              }
          }
      }
    it(line.toList, Nil)
  }

  def part1Score(line: String): Int =
    parse(line) match {
      case Left(Corrupted(_, ')')) => 3
      case Left(Corrupted(_, ']')) => 57
      case Left(Corrupted(_, '}')) => 1197
      case Left(Corrupted(_, '>')) => 25137
      case _                       => 0
    }

  def part1(input: List[String]): Int =
    input.map(part1Score).sum

  def part2Score(line: String): Long =
    parse(line) match {
      case Left(Incomplete(missing)) => completionScore(missing)
      case _                         => 0
    }

  def completionScore(missing: List[Char]): Long = {
    @tailrec
    def it(remain: List[Char], acc: Long): Long = {
      remain match {
        case Nil => acc
        case h :: t =>
          val points = h match {
            case ')' => 1
            case ']' => 2
            case '}' => 3
            case '>' => 4
          }
          it(t, acc * 5 + points)
      }
    }
    it(missing, 0)
  }

  def part2(input: List[String]): Long = {
    val sortedScores = input
      .map(parse)
      .collect { case Left(Incomplete(missing)) => completionScore(missing) }
      .sorted
    sortedScores(sortedScores.size / 2)
  }
}
