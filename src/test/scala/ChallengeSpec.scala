import TestUtils._
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
    val input = load("input/01.txt").map(_.toInt)
    countInc(input) mustBe 1462
    countIncSliding(input) mustBe 1497
  }
}
