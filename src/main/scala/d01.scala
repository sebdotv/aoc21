import scala.annotation.tailrec

object d01 {
  def countInc(list: List[Int]): Int = {
    @tailrec
    def it(l: List[Int], acc: Int, prev: Option[Int]): Int = {
      l match {
        case Nil => acc
        case h :: t =>
          val i = prev.map(p => if (p < h) 1 else 0).getOrElse(0)
          it(t, acc + i, Some(h))
      }
    }
    it(list, 0, None)
  }

  def countIncSliding(list: List[Int]): Int =
    countInc(slidingSum(list))

  def slidingSum(list: List[Int]): List[Int] =
    list.sliding(3).map(_.sum).toList
}
