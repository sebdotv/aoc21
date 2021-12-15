import aoc._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

object d11 {
  case class Grid(h: Int, w: Int, energyLevels: Vector[Int], totalFlashes: Int, steps: Int) {
    assert(energyLevels.size === h * w)

    def step(n: Int): Grid =
      (1 to n).foldLeft(this)((curr, _) => curr.step)

    def step: Grid = {
      val data = energyLevels.toArray
      val flashed = mutable.Set.empty[(Int, Int)]

      def adjacent(x: Int, y: Int) = for (b <- y - 1 to y + 1; a <- x - 1 to x + 1 if (a, b) =!= (x, y)) yield (a, b)
      def get_(x: Int, y: Int): Int = data(x + y * w)
      def set_(x: Int, y: Int)(value: Int): Unit = data(x + y * w) = value
      def inc_(x: Int, y: Int): Unit = {
//        println(s"inc_($x,$y)")
        val value = get_(x, y) + 1
        set_(x, y)(value)
        if (value > 9) flash_(x, y)
      }
      def flash_(x: Int, y: Int): Unit = {
        if (!flashed.contains((x, y))) {
          flashed.add((x, y))
//          println(s"flash_($x,$y)")
          adjacent(x, y).filter { case (x, y) => x >= 0 && x < w && y >= 0 && y < h }.foreach(inc)
        }
      }
      def set = (set_ _).tupled
      def inc = (inc_ _).tupled

      def coords = for (y <- 0 until h; x <- 0 until w) yield (x, y)

      coords.foreach(inc)

      flashed.foreach(set(_)(0))

      copy(energyLevels = data.toVector, totalFlashes = totalFlashes + flashed.size, steps = steps + 1)
    }

    def toStrLines: List[String] =
      energyLevels.map(_ + '0').map(_.toChar).mkString.grouped(w).toList

    override def toString: String =
      toStrLines.mkString("\n")

//    def inc(i: Int): Grid = {
//      val updated = copy(energyLevels = energyLevels.updated(i, energyLevels(i) + 1))
//      if (updated.energyLevels(i) > 9) updated.applyFlash(i)
//      else updated
//    }
//
//    def applyFlash(flash: Int): Grid = {
//      ???
//            val cells = for (x <- -1 to 1 , y < -1 to 1  if x !== 0 && y !== 0) yield (x,y)
//      //
//      //
//      //
//      //
//      //       {
//      //          val i = flash + x + y*w
//      //          if (i >= 0 && i < w*h) copy(energyLevels = energyLevels.updated(i, energyLevels(i) + 1)) else this
//      //
//      //      }
//    }

  }

  def parseGrid(input: List[String]): Grid = {
    val w = input.headOption.unsafeGet().length
    assert(input.forall(_.length === w))
    Grid(w, input.size, input.flatten.map(_ - '0').toVector, totalFlashes = 0, steps = 0)
  }

  @tailrec
  def firstSimultaneousFlashStep(input: Grid): Int = {
    val next = input.step
    if (next.energyLevels.forall(_ === 0)) next.steps
    else firstSimultaneousFlashStep(next)
  }
}
