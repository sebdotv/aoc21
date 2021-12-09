import cats.implicits._

object d09 {
  final case class Coord(x: Int, y: Int)
  final case class Heightmap(w: Int, h: Int, data: Vector[Int]) {
    assert(data.length === w * h)

    def get(c: Coord): Int =
      data(c.x + c.y * w)

    def adjacentLocations(c: Coord): List[Coord] =
      List(
        if (c.y > 0) List(c.copy(y = c.y - 1)) else Nil,
        if (c.y < h - 1) List(c.copy(y = c.y + 1)) else Nil,
        if (c.x > 0) List(c.copy(x = c.x - 1)) else Nil,
        if (c.x < w - 1) List(c.copy(x = c.x + 1)) else Nil
      ).flatten

    lazy val lowPoints: List[Coord] = {
      val coords = for (y <- 0 until h; x <- 0 until w) yield Coord(x, y)
      coords.filter(c => adjacentLocations(c).forall(get(c) < get(_))).toList
    }
  }

  def parse(input: List[String]): Heightmap = {
    val w = input.head.length
    val h = input.size
    assert(input.forall(_.length === w))
    Heightmap(w = w, h = h, input.flatMap(_.map(_ - '0')).toVector)
  }

  def totalRiskLevel(heightmap: Heightmap): Int =
    heightmap.lowPoints.map(heightmap.get(_) + 1).sum
}