package aoc

import cats.data.NonEmptyList

object Graph {
  type Path[V] = NonEmptyList[V]
  type Paths[V] = List[Path[V]]

  def dfs[V](edgesF: V => List[V])(visitedF: (Path[V], V) => Boolean)(start: V): Paths[V] = {
    def it(path: Path[V], results: Paths[V]): Paths[V] =
      edgesF(path.head) match {
        case Nil   => path :: results
        case edges => edges.filterNot(visitedF(path, _)).foldLeft(results)((b, next) => it(next :: path, b))
      }
    it(NonEmptyList.one(start), Nil).map(_.reverse)
  }
}
