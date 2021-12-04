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

  it should "do d04" in {
    import d04._
    val example = parse("""7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        |
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7""".stripMargin.splitLines)
    example.draw mustBe List(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1)
    example.boards.head.rows.head mustBe List(22, 13, 17, 11, 0)
    example.boards(2).rows(4) mustBe List(2, 0, 12, 3, 7)
    var board = example.boards(2)
    board.winningRows mustBe Nil
    board = List(7, 4, 9, 5, 11).foldLeft(board)(_.mark(_))
    board.winningRows mustBe Nil
    board = List(17, 23, 2, 0, 14, 21).foldLeft(board)(_.mark(_))
    board.winningRows mustBe Nil
    board = List(24).foldLeft(board)(_.mark(_))
    board.winningRows mustBe List(0)
    board.unmarkedSum mustBe 188
    score(example, 2)._1 mustBe 4512
    // input
    val input = parse(unsafeLoad("input/04.txt"))
    val scores = input.boards.indices.toList.map(i => i -> score(input, i)).sortBy { case (_, (_, draws)) => draws }
    assert(scores.head._2._1 === 41503)
  }
}
