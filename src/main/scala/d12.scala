import aoc._
import cats._
import cats.implicits._

object d12 {
  type Vertex = Cave
  type Edge = Set[Vertex]
  case class Path(path: List[Vertex]) {
    override def toString: String = path.mkString(",")
  }

  implicit val eqCave: Eq[Cave] = Eq.fromUniversalEquals
  case class Cave(name: String) {
    def small: Boolean = name(0).isLower
    override def toString: String = name
  }

  case class CaveSystem(edges: Set[Edge]) {
    import CaveSystem._
    val vertices: Set[Vertex] = edges.flatten
    assert(vertices.contains(start))
    assert(vertices.contains(end))

    def edgesFrom(vertex: Vertex): Set[Edge] =
      if (vertex === end) Set.empty else edges.filter(_.contains(vertex))

    def destinationsFrom(vertex: Vertex): Set[Vertex] =
      edgesFrom(vertex).map(_ - vertex).map(_.toList).map { case List(b) => b }

    def part1: List[Path] =
      Graph
        .dfs[Vertex](destinationsFrom(_).toList) { (path, v) =>
          v.small && path.contains_(v)
        }(start)
        .map(Path)

    def part2: List[Path] =
      Graph
        .dfs[Vertex](destinationsFrom(_).toList) { (path, v) =>
          v === start ||
          v.small && path.contains_(v) && path.filter(_.small).groupBy(identity).exists(_._2.size > 1)
        }(start)
        .map(Path)
  }
  object CaveSystem {
    private val start = Cave("start")
    private val end = Cave("end")
  }

  def parse(input: List[String]): CaveSystem =
    CaveSystem(Set(input.map(parseLine)).flatten)

  def parseLine(line: String): Edge = {
    val List(a, b) = line.split("-").map(Cave).toList
    Set(a, b)
  }
}
