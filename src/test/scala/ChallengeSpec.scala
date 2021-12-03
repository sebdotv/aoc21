import TestUtils._
import aoc._
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ChallengeSpec extends AnyFlatSpec with Matchers {
  "lazy coder" should "do d01" in {
    import d01._
    // examples
    val example = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    countInc(example) mustBe 7
    slidingSum(example) mustBe List(607, 618, 618, 617, 647, 716, 769, 792)
    countIncSliding(example) mustBe 5
    // input
    val input = unsafeLoad("input/01.txt").map(_.toInt)
    countInc(input) mustBe 1462
    countIncSliding(input) mustBe 1497
  }

  it should "do d02" in {
    import d02._
    // examples
    val example = List("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")
    finalState(example, interpreter1).pos mustBe (15, 10)
    finalState(example, interpreter2).pos mustBe (15, 60)
    // input
    val input = unsafeLoad("input/02.txt")
    finalState(input, interpreter1).pos mustBe (2105, 807)
    finalState(input, interpreter2).pos mustBe (2105, 757618)
  }

  it should "do d03" in {
    import d03._
    // examples
    val example = NonEmptyList.of("00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010")
    gammaBits(example) mustBe "10110"
    gamma(example) mustBe 22
    epsilonBits(example) mustBe "01001"
    epsilon(example) mustBe 9
    power(example) mustBe 198
    // input
    val input = NonEmptyList.fromList(unsafeLoad("input/03.txt")).unsafeGet()
    power(input) mustBe 4138664
    // part 2
    o2Gen(example) mustBe 23
    co2Scrub(example) mustBe 10
    lifeSupport(example) mustBe 230
    lifeSupport(input) mustBe 4273224
  }
}
