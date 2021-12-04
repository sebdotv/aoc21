import scala.annotation.tailrec
import cats.implicits._

object d04 {
  final case class Input(draw: List[Int], boards: List[Board])
  final case class Board(rows: List[List[Int]], marked: List[List[Boolean]], draws: Int) {
    assert(rows.forall(_.length === 5))
    assert(marked.forall(_.length === 5))
    def mark(n: Int): Board = {
      val marks = for (row <- 0 until 5; col <- 0 until 5; if rows(row)(col) === n) yield (row, col)
      val newMarked = marks.foldLeft(marked) { case (curr, (row, col)) =>
        curr.updated(row, curr(row).updated(col, true))
      }
      copy(marked = newMarked, draws = draws + 1)
    }
    def winningRows: List[Int] =
      (for (row <- 0 until 5; if (0 until 5).forall(col => marked(row)(col))) yield row).toList
    def winningCols: List[Int] =
      (for (col <- 0 until 5; if (0 until 5).forall(row => marked(row)(col))) yield col).toList
    def isWinner: Boolean =
      winningRows.nonEmpty || winningCols.nonEmpty
    def unmarkedSum: Int =
      (for (row <- 0 until 5; col <- 0 until 5; if !marked(row)(col)) yield rows(row)(col)).sum
  }
  object Board {
    def fresh(rows: List[List[Int]]): Board =
      Board(rows, rows.map(_.map(_ => false)), 0)
    def parse(rows: List[String]): Board =
      Board.fresh(rows.map(_.split("""\s+""").toList.filterNot(_.isEmpty).map(_.toInt)))
  }

  def parse(lines: List[String]): Input = {
    val n :: rest = lines
    @tailrec
    def it(l: List[String], acc: List[Board]): List[Board] =
      l match {
        case Nil     => acc
        case "" :: t => it(t, acc)
        case _ =>
          val (rows, rest) = l.splitAt(5)
          val board = Board.parse(rows)
          it(rest, board :: acc)
      }
    Input(n.split(",").map(_.toInt).toList, it(rest, Nil).reverse)
  }

  /** @return
    *   score and number of draws
    */
  def score(input: Input, b: Int): (Int, Int) = {
    @tailrec
    def it(remaining: List[Int], board: Board): (Int, Int) =
      remaining match {
        case h :: t =>
          val updated = board.mark(h)
          if (updated.isWinner) (h * updated.unmarkedSum, board.draws)
          else it(t, updated)
        case Nil => (0, board.draws)
      }
    it(input.draw, input.boards(b))
  }

}
