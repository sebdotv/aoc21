import aoc._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.tailrec

object d03 {
  type Input = NonEmptyList[String]

  def power(input: Input): Int =
    gamma(input) * epsilon(input)
  def lifeSupport(input: Input): Int =
    o2Gen(input) * co2Scrub(input)

  def gamma(input: Input): Int =
    bitsToInt(gammaBits(input))
  def epsilon(input: Input): Int =
    bitsToInt(epsilonBits(input))
  def o2Gen(input: Input): Int =
    bitsToInt(selectedBits(input, o2GenSelector))
  def co2Scrub(input: Input): Int =
    bitsToInt(selectedBits(input, co2ScrubSelector))

  def gammaBits(input: Input): String = {
    val len = input.head.length
    assert(input.forall(_.length === len))
    (0 until len).map { i =>
      input
        .map(_.charAt(i))
        .groupBy(identity)
        .toList
        .sortBy(-_._2.size)
        .headOption
        .unsafeGet()
        ._1
    }.mkString
  }
  def epsilonBits(input: Input): String =
    swapBits(gammaBits(input))

  type Selector = List[Char] => Char
  private val o2GenSelector: Selector = _.headOption.unsafeGet()
  private val co2ScrubSelector: Selector = _.lastOption.unsafeGet()

  private def selectedBits(input: Input, selector: Selector): String = {
    val len = input.head.length
    assert(input.forall(_.length === len))

    @tailrec
    def it(i: Int, l: List[String]): String = {
      val options = l
        .map(_.charAt(i))
        .groupBy(identity)
        .toList
        .sortBy { case (c, values) => (-values.size, -c) } // most common, 1 first
        .map { case (c, _) => c }
      val selectedBit = selector(options)
      val matching = l.filter(_.charAt(i) === selectedBit)
      matching match {
        case List(line) => line
        case _          => it(i + 1, matching)
      }
    }

    it(0, input.toList).mkString
  }

  def bitsToInt(bits: String): Int =
    Integer.parseInt(bits, 2)

  def swapBits(bits: String): String =
    bits.map {
      case '0' => '1'
      case '1' => '0'
    }
}
