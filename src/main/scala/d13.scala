import aoc.trigo._
import cats.implicits._

import scala.annotation.tailrec

object d13 {
  sealed trait Fold
  object Fold {
    private val regex = """fold along ([xy])=(\d+)""".r
    def parse(s: String): Fold = {
      val regex(dir, pos) = s
      val value = pos.toInt
      dir match {
        case "x" => VertFold(x = value)
        case "y" => HorizFold(y = value)
      }
    }
  }
  final case class HorizFold(y: Int) extends Fold
  final case class VertFold(x: Int) extends Fold
  final case class Paper(w: Int, h: Int, dots: Set[Coord], folds: List[Fold]) {
    def fold: Option[Paper] =
      folds match {
        case head :: rest =>
          head match {
            case HorizFold(foldY) =>
              val (unchanged, toMirror) = dots.partition(_.y < foldY)
              Paper(w, h / 2, unchanged ++ toMirror.map { c => c.copy(y = 2 * foldY - c.y) }, rest).some
            case VertFold(foldX) =>
              val (unchanged, toMirror) = dots.partition(_.x < foldX)
              Paper(w / 2, h, unchanged ++ toMirror.map { c => c.copy(x = 2 * foldX - c.x) }, rest).some
          }
        case Nil => None
      }

    @tailrec
    def foldN(n: Int): Option[Paper] =
      if (n === 0) Some(this)
      else
        fold match {
          case None    => None
          case Some(p) => p.foldN(n - 1)
        }

    @tailrec
    def foldAll: Paper =
      fold match {
        case None    => this
        case Some(p) => p.foldAll
      }

    def toStrLines: List[String] =
      (0 until h).map { y =>
        (0 until w).map(x => if (dots.contains(Coord(x, y))) '#' else '.').mkString
      }.toList

    override def toString: String = toStrLines.mkString("\n")
  }

  def parse(input: List[String]): Paper = {
    val (dots, folds) = input.splitAt(input.indexOf(""))
    val coords = dots.map(Coord.parse)
    val (minX, minY, maxX, maxY) = Coord.computeExtent(coords)
    assert((minX, minY) === (0, 0))
    Paper(w = maxX + 1, h = maxY + 1, coords.toSet, folds.drop(1).map(Fold.parse))
  }
}
