import TestUtils._
import aoc._
import aoc.trigo.Vect
import cats.data.NonEmptyList
import cats.implicits.toShow
import org.scalatest.Inside._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.annotation.tailrec

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

  it should "do d09" in {
    import d09._
    val example = parse("""2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin.splitLines)
    example.lowPoints.map(example.get) mustBe List(1, 0, 5, 5)
    totalRiskLevel(example) mustBe 15
    // input
    val input = parse(unsafeLoad("input/09.txt"))
    totalRiskLevel(input) mustBe 436
    // part 2
    sortedBasinSizes(example) mustBe List(3, 9, 9, 14)
    part2(example) mustBe 1134
    part2(input) mustBe 1317792
  }

  it should "do d10" in {
    import d10._
    // legal
    parse("()").left.toOption mustBe None
    parse("[]").left.toOption mustBe None
    parse("([])").left.toOption mustBe None
    parse("{()()()}").left.toOption mustBe None
    parse("<([{}])>").left.toOption mustBe None
    parse("[<>({}){}[([])<>]]").left.toOption mustBe None
    parse("(((((((((())))))))))").left.toOption mustBe None
    // corrupted
    parse("(]").isCorrupted mustBe true
    parse("{()()()>").isCorrupted mustBe true
    parse("(((()))}").isCorrupted mustBe true
    parse("<([]){()}[{}])").isCorrupted mustBe true
    // example
    val example = """[({(<(())[]>[[{[]{<()<>>
                    |[(()[<>])]({[<{<<[]>>(
                    |{([(<{}[<>[]}>{[]{[(<()>
                    |(((({<>}<{<{<>}{[]{[]{}
                    |[[<[([]))<([[{}[[()]]]
                    |[{[{({}]{}}([{[{{{}}([]
                    |{<[[]]>}<{[{[{[]{()[[[]
                    |[<(<(<(<{}))><([]([]()
                    |<{([([[(<>()){}]>(<<{{
                    |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.splitLines
    val parsed = example.map(parse)
    parsed.count(_.isCorrupted) mustBe 5
    parsed.count(_.isIncomplete) mustBe example.size - 5
    part1(example) mustBe 26397
    // input
    val input = unsafeLoad("input/10.txt")
    part1(input) mustBe 323613
    // part 2
    parsed.collect { case Left(Incomplete(missing)) => missing.mkString } mustBe List(
      "}}]])})]",
      ")}>]})",
      "}}>}>))))",
      "]]}}]}]}>",
      "])}>"
    )
    completionScore("}}]])})]".toList) mustBe 288957
    completionScore(")}>]})".toList) mustBe 5566
    completionScore("}}>}>))))".toList) mustBe 1480781
    completionScore("]]}}]}]}>".toList) mustBe 995444
    completionScore("])}>".toList) mustBe 294
    part2(example) mustBe 288957
    part2(input) mustBe 3103006161L
  }

  it should "do d11" in {
    import d11._
    val example0 = parseGrid("""11111
        |19991
        |19191
        |19991
        |11111""".stripMargin.splitLines)
    example0.nSteps(1).toString mustBe
      """34543
        |40004
        |50005
        |40004
        |34543""".stripMargin
    example0.nSteps(2).toString mustBe
      """45654
        |51115
        |61116
        |51115
        |45654""".stripMargin
    val example = parseGrid("""5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526""".stripMargin.splitLines)
    example.nSteps(1).toString mustBe
      """6594254334
        |3856965822
        |6375667284
        |7252447257
        |7468496589
        |5278635756
        |3287952832
        |7993992245
        |5957959665
        |6394862637""".stripMargin
    example.nSteps(100).toString mustBe
      """0397666866
        |0749766918
        |0053976933
        |0004297822
        |0004229892
        |0053222877
        |0532222966
        |9322228966
        |7922286866
        |6789998766""".stripMargin
    example.nSteps(100).totalFlashes mustBe 1656
    // input
    val input = parseGrid(unsafeLoad("input/11.txt"))
    input.nSteps(100).totalFlashes mustBe 1661
    // part 2
    firstSimultaneousFlashStep(example) mustBe 195
    firstSimultaneousFlashStep(input) mustBe 334
  }

  it should "do d12" in {
    import d12._
    // examples
    val example1 = parse("""start-A
                          |start-b
                          |A-c
                          |A-b
                          |b-d
                          |A-end
                          |b-end""".stripMargin.splitLines)
    example1.vertices must have size 6
    example1.edges must have size 7
    example1.part1.map(_.toString) must contain theSameElementsAs
      """start,A,b,A,c,A,end
        |start,A,b,A,end
        |start,A,b,end
        |start,A,c,A,b,A,end
        |start,A,c,A,b,end
        |start,A,c,A,end
        |start,A,end
        |start,b,A,c,A,end
        |start,b,A,end
        |start,b,end""".stripMargin.splitLines
    val example2 = parse("""dc-end
                           |HN-start
                           |start-kj
                           |dc-start
                           |dc-HN
                           |LN-dc
                           |HN-end
                           |kj-sa
                           |kj-HN
                           |kj-dc""".stripMargin.splitLines)
    example2.part1.map(_.toString) must contain theSameElementsAs
      """start,HN,dc,HN,end
        |start,HN,dc,HN,kj,HN,end
        |start,HN,dc,end
        |start,HN,dc,kj,HN,end
        |start,HN,end
        |start,HN,kj,HN,dc,HN,end
        |start,HN,kj,HN,dc,end
        |start,HN,kj,HN,end
        |start,HN,kj,dc,HN,end
        |start,HN,kj,dc,end
        |start,dc,HN,end
        |start,dc,HN,kj,HN,end
        |start,dc,end
        |start,dc,kj,HN,end
        |start,kj,HN,dc,HN,end
        |start,kj,HN,dc,end
        |start,kj,HN,end
        |start,kj,dc,HN,end
        |start,kj,dc,end""".stripMargin.splitLines
    val example3 = parse("""fs-end
                           |he-DX
                           |fs-he
                           |start-DX
                           |pj-DX
                           |end-zg
                           |zg-sl
                           |zg-pj
                           |pj-he
                           |RW-he
                           |fs-DX
                           |pj-RW
                           |zg-RW
                           |start-pj
                           |he-WI
                           |zg-he
                           |pj-fs
                           |start-RW""".stripMargin.splitLines)
    example3.part1 must have size 226
    // input
    val input = parse(unsafeLoad("input/12.txt"))
    input.part1 must have size 4413
    // part 2
    example1.part2.map(_.toString) must contain theSameElementsAs
      """start,A,b,A,b,A,c,A,end
        |start,A,b,A,b,A,end
        |start,A,b,A,b,end
        |start,A,b,A,c,A,b,A,end
        |start,A,b,A,c,A,b,end
        |start,A,b,A,c,A,c,A,end
        |start,A,b,A,c,A,end
        |start,A,b,A,end
        |start,A,b,d,b,A,c,A,end
        |start,A,b,d,b,A,end
        |start,A,b,d,b,end
        |start,A,b,end
        |start,A,c,A,b,A,b,A,end
        |start,A,c,A,b,A,b,end
        |start,A,c,A,b,A,c,A,end
        |start,A,c,A,b,A,end
        |start,A,c,A,b,d,b,A,end
        |start,A,c,A,b,d,b,end
        |start,A,c,A,b,end
        |start,A,c,A,c,A,b,A,end
        |start,A,c,A,c,A,b,end
        |start,A,c,A,c,A,end
        |start,A,c,A,end
        |start,A,end
        |start,b,A,b,A,c,A,end
        |start,b,A,b,A,end
        |start,b,A,b,end
        |start,b,A,c,A,b,A,end
        |start,b,A,c,A,b,end
        |start,b,A,c,A,c,A,end
        |start,b,A,c,A,end
        |start,b,A,end
        |start,b,d,b,A,c,A,end
        |start,b,d,b,A,end
        |start,b,d,b,end
        |start,b,end""".stripMargin.splitLines
    example2.part2 must have size 103
    example3.part2 must have size 3509
    input.part2 must have size 118803
  }

  it should "do d13" in {
    import d13._
    val example = parse("""
        |6,10
        |0,14
        |9,10
        |0,3
        |10,4
        |4,11
        |6,0
        |6,12
        |4,1
        |0,13
        |10,12
        |3,4
        |3,0
        |8,4
        |1,10
        |2,14
        |8,10
        |9,0
        |
        |fold along y=7
        |fold along x=5""".stripMargin.splitLines)
    example.fold.unsafeGet().toString mustBe
      """#.##..#..#.
        |#...#......
        |......#...#
        |#...#......
        |.#.#..#.###
        |...........
        |...........""".stripMargin
    example.fold.unsafeGet().dots.size mustBe 17
    example.foldN(2).unsafeGet().toString mustBe
      """#####
        |#...#
        |#...#
        |#...#
        |#####
        |.....
        |.....""".stripMargin
    // input
    val input = parse(unsafeLoad("input/13.txt"))
    input.fold.unsafeGet().dots.size mustBe 695
    // part 2
    input.foldAll.toString mustBe
      """.##....##.####..##..#....#..#.###....##.
        |#..#....#....#.#..#.#....#..#.#..#....#.
        |#.......#...#..#....#....#..#.#..#....#.
        |#.##....#..#...#.##.#....#..#.###.....#.
        |#..#.#..#.#....#..#.#....#..#.#....#..#.
        |.###..##..####..###.####..##..#.....##..""".stripMargin
  }

  it should "do d14" in {
    import d14._
    val example = parse("""
        |NNCB
        |
        |CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C""".stripMargin.splitLines)
    example.stepN(1).polymer.mkString mustBe "NCNBCHB"
    example.stepN(2).polymer.mkString mustBe "NBCCNBBBCBHCB"
    example.stepN(3).polymer.mkString mustBe "NBBBCNCCNBBNBNBBCHBHHBCHB"
    example.stepN(4).polymer.mkString mustBe "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    example.stepN(5).polymer.size mustBe 97
    example.stepN(10).polymer.size mustBe 3073
    example.stepN(10).elementCounts mustBe Map('B' -> 1749, 'C' -> 298, 'H' -> 161, 'N' -> 865)
    example.stepN(10).maxMinusMin mustBe 1588
    // input
    val input = parse(unsafeLoad("input/14.txt"))
    input.stepN(10).maxMinusMin mustBe 3143
    // part 2
    input.fast.stepN(10).maxMinusMin mustBe 3143
    input.fast.stepN(40).maxMinusMin mustBe 4110215602456L
  }

  it should "do d15" in {
    import d15._
    val solve = solveAStar _
    // example
    val example = parse("""
      |1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin.splitLines)
    solve(example) mustBe 40
    // input
    val input = parse(unsafeLoad("input/15.txt"))
    solve(input) mustBe 523
    // part 2
    solve(growForPart2(example)) mustBe 315
    val fullInput = growForPart2(input)
    time(solve(fullInput)) mustBe 2876
  }

  it should "do d16" in {
    import d16._
    // examples
    parse("D2FE28") mustBe Packet(6, 4, LiteralValuePacketData(2021))
    inside(parse("38006F45291200")) { packet =>
      packet.version mustBe 1
      packet.typeID mustBe 6
      inside(packet.data) { case OperatorPacketData(subPackets) =>
        subPackets.map(_.data) mustBe List(LiteralValuePacketData(10), LiteralValuePacketData(20))
      }
    }
    inside(parse("EE00D40C823060")) { packet =>
      packet.version mustBe 7
      packet.typeID mustBe 3
      inside(packet.data) { case OperatorPacketData(subPackets) =>
        subPackets.map(_.data) mustBe List(LiteralValuePacketData(1), LiteralValuePacketData(2), LiteralValuePacketData(3))
      }
    }
    inside(parseAsNode("8A004A801A8002F478")) { case n @ Operator(4, List(Operator(1, List(Operator(5, List(LiteralValue(6, _))))))) =>
      n.versionSum mustBe 16
    }
    inside(parseAsNode("620080001611562C8802118E34")) {
      case n @ Operator(
            3,
            List(
              Operator(_, List(LiteralValue(_, _), LiteralValue(_, _))),
              Operator(_, List(LiteralValue(_, _), LiteralValue(_, _)))
            )
          ) =>
        n.versionSum mustBe 12
    }
    inside(parseAsNode("C0015000016115A2E0802F182340")) {
      case n @ Operator(
            _,
            List(
              Operator(_, List(LiteralValue(_, _), LiteralValue(_, _))),
              Operator(_, List(LiteralValue(_, _), LiteralValue(_, _)))
            )
          ) =>
        n.versionSum mustBe 23
    }
    inside(parseAsNode("A0016C880162017C3686B18A3D4780")) {
      case n @ Operator(
            _,
            List(
              Operator(_, List(Operator(_, List(LiteralValue(_, _), LiteralValue(_, _), LiteralValue(_, _), LiteralValue(_, _), LiteralValue(_, _)))))
            )
          ) =>
        n.versionSum mustBe 31
    }
    // input
    val input = parse(unsafeLoadLine("input/16.txt"))
    input.toNode.versionSum mustBe 866
    // part 2
    parse("C200B40A82").value mustBe 3
    parse("04005AC33890").value mustBe 54
    parse("880086C3E88112").value mustBe 7
    parse("CE00C43D881120").value mustBe 9
    parse("D8005AC2A8F0").value mustBe 1
    parse("F600BC2D8F").value mustBe 0
    parse("9C005AC2F8F0").value mustBe 0
    parse("9C0141080250320F1802104A08").value mustBe 1
    input.value mustBe 1392637195518L
  }

  it should "do d17" in {
    import d17._
    val example = parse("target area: x=20..30, y=-10..-5")
    State.initial(example)(Vect(7, 2)).runUntilHitOrMiss.toGrid mustBe
      """.............#....#............
        |.......#..............#........
        |...............................
        |S........................#.....
        |...............................
        |...............................
        |...........................#...
        |...............................
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTT#TT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT""".stripMargin
    State.initial(example)(Vect(6, 3)).runUntilHitOrMiss.toGrid mustBe
      """...............#..#............
        |...........#........#..........
        |...............................
        |......#..............#.........
        |...............................
        |...............................
        |S....................#.........
        |...............................
        |...............................
        |...............................
        |.....................#.........
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................T#TTTTTTTTT
        |....................TTTTTTTTTTT""".stripMargin
    State.initial(example)(Vect(9, 0)).runUntilHitOrMiss.toGrid mustBe
      """S........#.....................
        |.................#.............
        |...............................
        |........................#......
        |...............................
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTT#
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT
        |....................TTTTTTTTTTT""".stripMargin
    State.initial(example)(Vect(17, -4)).runUntilHitOrMiss.stepN(2).toGrid mustBe
      """S..............................................................
        |...............................................................
        |...............................................................
        |...............................................................
        |.................#.............................................
        |....................TTTTTTTTTTT................................
        |....................TTTTTTTTTTT................................
        |....................TTTTTTTTTTT................................
        |....................TTTTTTTTTTT................................
        |....................TTTTTTTTTTT..#.............................
        |....................TTTTTTTTTTT................................
        |...............................................................
        |...............................................................
        |...............................................................
        |...............................................................
        |................................................#..............
        |...............................................................
        |...............................................................
        |...............................................................
        |...............................................................
        |...............................................................
        |...............................................................
        |..............................................................#""".stripMargin
    part1(example) mustBe Vect(6, 9) -> 45
    // input
    val input = parse(unsafeLoadLine("input/17.txt"))
    part1(input) mustBe Vect(17, 99) -> 4950
    // part 2
    part2(example).size mustBe 112
    part2(input).size mustBe 1477
  }

  it should "do d18" in {
    import d18._
    def testExplode(s: String, pairText: String, expected: String): Unit = {
      inside(toMutable(parse(s))) { case root: MutablePairNode =>
        root.toText mustBe s
        val List(pair) = root.allNodes.collect { case pair: MutablePairNode => pair }.filter(_.toText === pairText)
        explode(pair)
        root.toText mustBe expected
      }
    }
    def testSplit(s: String, rnText: String, expected: String): Unit = {
      inside(toMutable(parse(s))) { case root: MutablePairNode =>
        root.toText mustBe s
        val List(rn) = root.allNodes.collect { case rn: MutableRegularNumberNode => rn }.filter(_.toText === rnText)
        split(rn)
        root.toText mustBe expected
      }
    }
    testExplode("[[[[[9,8],1],2],3],4]", "[9,8]", "[[[[0,9],2],3],4]")
    testExplode("[7,[6,[5,[4,[3,2]]]]]", "[3,2]", "[7,[6,[5,[7,0]]]]")
    testExplode("[[6,[5,[4,[3,2]]]],1]", "[3,2]", "[[6,[5,[7,0]]],3]")
    testExplode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[7,3]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    testExplode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[3,2]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
    testSplit("[[[[0,7],4],[15,[0,13]]],[1,1]]", "15", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
    testSplit("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "13", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
    add(parse("[[[[4,3],4],4],[7,[[8,4],9]]]"), parse("[1,1]")).show mustBe "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    finalSum("""[1,1]
               |[2,2]
               |[3,3]
               |[4,4]""".stripMargin.splitLines.map(parse)).show mustBe "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    finalSum("""[1,1]
               |[2,2]
               |[3,3]
               |[4,4]
               |[5,5]""".stripMargin.splitLines.map(parse)).show mustBe "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    finalSum("""[1,1]
               |[2,2]
               |[3,3]
               |[4,4]
               |[5,5]
               |[6,6]""".stripMargin.splitLines.map(parse)).show mustBe "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    finalSum("""[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
        |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
        |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
        |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
        |[7,[5,[[3,8],[1,4]]]]
        |[[2,[2,2]],[8,[8,1]]]
        |[2,9]
        |[1,[[[9,3],9],[[9,0],[0,7]]]]
        |[[[5,[7,4]],7],1]
        |[[[[4,2],2],6],[8,7]]""".stripMargin.splitLines.map(parse)).show mustBe "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    parse("[9,1]").magnitude mustBe 29
    parse("[1,9]").magnitude mustBe 21
    parse("[[9,1],[1,9]]").magnitude mustBe 129
    parse("[[1,2],[[3,4],5]]").magnitude mustBe 143
    parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude mustBe 1384
    parse("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude mustBe 445
    parse("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude mustBe 791
    parse("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude mustBe 1137
    parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude mustBe 3488
    val example = finalSum("""[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
        |[[[5,[2,8]],4],[5,[[9,9],0]]]
        |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
        |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
        |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
        |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
        |[[[[5,4],[7,7]],8],[[8,3],8]]
        |[[9,3],[[9,9],[6,[4,9]]]]
        |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
        |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin.splitLines.map(parse))
    example.show mustBe "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
    example.magnitude mustBe 4140
    // input
    val input = finalSum(unsafeLoad("input/18.txt").map(parse))
    input.magnitude mustBe 3574
  }
}
