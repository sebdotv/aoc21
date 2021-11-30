import TestUtils._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ChallengeSpec extends AnyFlatSpec with Matchers {
  "lazy coder" should "do d01" in {
    import d01._
    // examples
    massToFuel(12) mustBe 2
    massToFuel(14) mustBe 2
    massToFuel(1969) mustBe 654
    massToFuel(100756) mustBe 33583
    // input
    val input = load("input/01.txt")
    result(input) mustBe 3560353
    result(input) mustBe 5337642
  }
}
