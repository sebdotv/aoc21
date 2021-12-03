import cats.data.NonEmptyList
import cats.implicits._
import aoc._

object d03 {
  type Input = NonEmptyList[String]

  def power(input: Input): Int =
    gamma(input) * epsilon(input)

  def gamma(input: Input): Int =
    bitsToInt(gammaBits(input))
  def epsilon(input: Input): Int =
    bitsToInt(epsilonBits(input))

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

  def bitsToInt(bits: String): Int =
    Integer.parseInt(bits, 2)

  def swapBits(bits: String): String =
    bits.map {
      case '0' => '1'
      case '1' => '0'
    }
}
