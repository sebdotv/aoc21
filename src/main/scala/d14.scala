import aoc._
import cats.implicits._

import scala.annotation.tailrec

object d14 {
  type Element = Char
  type Pair = List[Element]

  implicit val orderingPair: Ordering[Pair] = Ordering.fromLessThan { case (a, b) => a.mkString < b.mkString }

  final case class InsertionRule(pair: Pair, inserted: Element) {
    assert(pair.size === 2)
    def resultingPairs: List[Pair] =
      pair.patch(1, List(inserted), 0).sliding(2).toList
  }
  object InsertionRule {
    private val regex = """(..) -> (.)""".r
    def parse(s: String): InsertionRule = {
      val regex(pair, i) = s
      val Seq(inserted) = i.toList
      InsertionRule(pair = pair.toList, inserted = inserted)
    }
  }

  trait Polymerization {
    def elementCounts: Map[Element, Long]
    def maxMinusMin: Long = {
      val counts = elementCounts.values
      counts.max - counts.min
    }
  }

  final case class SlowPolymerization(polymer: List[Element], rules: List[InsertionRule]) extends Polymerization {
    def step: SlowPolymerization =
      copy(polymer =
        polymer
          .sliding(2)
          .toList
          .map { pair =>
            rules.find(_.pair === pair).map(_.inserted) match {
              case Some(inserted) => pair.patch(1, List(inserted), 0)
              case None           => pair
            }
          }
          .flatMap(_.dropRight(1)) ++ polymer.takeRight(1)
      )
    @tailrec
    def stepN(n: Int): SlowPolymerization =
      if (n === 0) this else step.stepN(n - 1)

    def elementCounts: Map[Element, Long] =
      polymer.groupBy(identity).view.mapValues(_.size.toLong).toMap

    def pairCounts: Map[Pair, Long] =
      polymer.sliding(2).toList.groupBy(identity).view.mapValues(_.size.toLong).toMap

    def fast: FastPolymerization =
      FastPolymerization(polymer.last, pairCounts, rules)
  }

  final case class FastPolymerization(finalElement: Element, pairCounts: Map[Pair, Long], rules: List[InsertionRule]) extends Polymerization {
    def step: FastPolymerization =
      copy(pairCounts =
        pairCounts
          .map { case (pair, count) =>
            rules.find(_.pair === pair).toList.flatMap { _.resultingPairs.map(_ -> count) }
          }
          .foldLeft(Map.empty[Pair, Long]) { case (acc, currPairCounts) =>
            currPairCounts.foldLeft(acc) { case (acc2, (pair, count)) =>
              acc2.updatedWith(pair) {
                case Some(value) => Some(value + count)
                case None        => Some(count)
              }
            }
          }
      )
    @tailrec
    def stepN(n: Int): FastPolymerization =
      if (n === 0) this else step.stepN(n - 1)

    def elementCounts: Map[Element, Long] =
      pairCounts.foldLeft(Map(finalElement -> 1L)) { case (acc, (pair, count)) =>
        acc.updatedWith(pair.headOption.unsafeGet()) {
          case Some(value) => Some(value + count)
          case None        => Some(count)
        }
      }
  }

  def parse(input: List[String]): SlowPolymerization =
    input match {
      case template :: "" :: rules => SlowPolymerization(template.toList, rules.map(InsertionRule.parse))
    }
}
