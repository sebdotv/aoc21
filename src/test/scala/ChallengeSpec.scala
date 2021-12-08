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
    score(example, example.boards(2))._1 mustBe 4512
    val exampleScores = scores(example)
    exampleScores.firstToWin mustBe ((4512, 12), 2)
    exampleScores.lastToWin mustBe ((1924, 15), 1)
    // input
    val input = parse(unsafeLoad("input/04.txt"))
    val inputScores = scores(input)
    inputScores.firstToWin._1._1 mustBe 41503
    inputScores.lastToWin._1._1 mustBe 3178
  }

  it should "do d05" in {
    import d05._
    // example
    val exampleData = """0,9 -> 5,9
                        |8,0 -> 0,8
                        |9,4 -> 3,4
                        |2,2 -> 2,1
                        |7,0 -> 7,4
                        |6,4 -> 2,0
                        |0,9 -> 2,9
                        |3,4 -> 1,4
                        |0,0 -> 8,8
                        |5,5 -> 8,2""".stripMargin.splitLines
    val example1 = parse(exampleData, withDiagonals = false)
    example1.toStrLines mustBe """.......1..
                                |..1....1..
                                |..1....1..
                                |.......1..
                                |.112111211
                                |..........
                                |..........
                                |..........
                                |..........
                                |222111....""".stripMargin.splitLines
    example1.countDangerous mustBe 5
    // input
    val inputData = unsafeLoad("input/05.txt")
    val input1 = parse(inputData, withDiagonals = false)
    input1.countDangerous mustBe 4728
    // part 2
    val example2 = parse(exampleData, withDiagonals = true)
    example2.toStrLines mustBe """1.1....11.
                                 |.111...2..
                                 |..2.1.111.
                                 |...1.2.2..
                                 |.112313211
                                 |...1.2....
                                 |..1...1...
                                 |.1.....1..
                                 |1.......1.
                                 |222111....""".stripMargin.splitLines
    example2.countDangerous mustBe 12
    val input2 = parse(inputData, withDiagonals = true)
    input2.countDangerous mustBe 17717
  }

  it should "do d06" in {
    import d06._
    val example = FastSim.fromIndivs(Vector(3, 4, 3, 1, 2))
    example.skipDays(18).indivCount mustBe 26
    example.skipDays(80).indivCount mustBe 5934
    // input
    val input = FastSim.fromIndivs(unsafeLoadLine("input/06.txt").split(",").map(_.toInt).toVector)
    input.skipDays(80).indivCount mustBe 365862
    // part 2
    example.skipDays(256).indivCount mustBe 26984457539L
    input.skipDays(256).indivCount mustBe 1653250886439L
  }

  it should "do d07" in {
    import d07._
    // example
    val exampleData = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)
    val example1 = Puzzle(exampleData, part1FuelFn)
    example1.fuelNeededTo(2) mustBe 37
    example1.fuelNeededTo(1) mustBe 41
    example1.fuelNeededTo(3) mustBe 39
    example1.fuelNeededTo(10) mustBe 71
    example1.bruteForceBest mustBe Target(pos = 2, fuel = 37)
    // input
    val inputData = unsafeLoadLine("input/07.txt").split(",").map(_.toInt).toList
    val input1 = Puzzle(inputData, part1FuelFn)
    input1.bruteForceBest mustBe Target(pos = 343, fuel = 353800)
    // part 2
    part2FuelFn(16, 5) mustBe 66
    part2FuelFn(1, 5) mustBe 10
    part2FuelFn(2, 5) mustBe 6
    part2FuelFn(0, 5) mustBe 15
    part2FuelFn(4, 5) mustBe 1
    part2FuelFn(2, 5) mustBe 6
    part2FuelFn(7, 5) mustBe 3
    part2FuelFn(1, 5) mustBe 10
    part2FuelFn(2, 5) mustBe 6
    part2FuelFn(14, 5) mustBe 45
    val example2 = Puzzle(exampleData, part2FuelFn)
    example2.fuelNeededTo(5) mustBe 168
    example2.fuelNeededTo(2) mustBe 206
    val input2 = Puzzle(inputData, part2FuelFn)
    input2.bruteForceBest mustBe Target(pos = 480, fuel = 98119739)
  }

  it should "do d08" in {
    import d08._
    val example1 = Entry.parse("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
    val example2 = List(
      "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ).map(Entry.parse)
    countEasy(example2) mustBe 26
    // input
    val input = unsafeLoad("input/08.txt").map(Entry.parse)
    countEasy(input) mustBe 237
    // part 2
    solve(example1) mustBe 5353
    example2.map(solve) mustBe List(8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315)
    part2(example2) mustBe 61229
    part2(input) mustBe 1009098
  }
}
