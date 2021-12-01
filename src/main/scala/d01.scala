object d01 {
  def countInc(list: List[Int]): Int =
    list.drop(1).zip(list).count { case (a, b) => a > b }

  def countIncSliding(list: List[Int]): Int =
    countInc(slidingSum(list))

  def slidingSum(list: List[Int]): List[Int] =
    list.sliding(3).map(_.sum).toList
}
