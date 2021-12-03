import aoc._
import cats.data.NonEmptyList
import cats.implicits._

import scala.annotation.tailrec

object d03 {
  type Bit = Char
  type Bits = String
  type Input = NonEmptyList[Bits]

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

  def gammaBits(input: Input): Bits = {
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
  def epsilonBits(input: Input): Bits =
    swapBits(gammaBits(input))

  type Selector = NonEmptyList[Bit] => Bit
  private val o2GenSelector: Selector = _.head
  private val co2ScrubSelector: Selector = _.last

  private def selectedBits(input: Input, selector: Selector): Bits = {
    val len = input.head.length
    assert(input.forall(_.length === len))

    @tailrec
    def it(i: Int, l: Input): Bits =
      l match {
        case NonEmptyList(bits, Nil) => bits
        case _ =>
          val optionsL = l
            .map(_.charAt(i))
            .groupBy(identity)
            .toList
            .sortBy { case (c, values) => (-values.size, -c) } // most common, 1 first
            .map { case (c, _) => c }
          val options = NonEmptyList.fromList(optionsL).unsafeGet() // guaranteed to be non-empty
          val selectedBit = selector(options)
          val matchingL = l.filter(_.charAt(i) === selectedBit)
          val matching = NonEmptyList.fromList(matchingL).unsafeGet() // guaranteed to be non-empty
          it(i + 1, matching)
      }

    it(0, input).mkString
  }

  def bitsToInt(bits: Bits): Int =
    Integer.parseInt(bits, 2)

  def swapBits(bits: Bits): Bits =
    bits.map {
      case '0' => '1'
      case '1' => '0'
    }
}
