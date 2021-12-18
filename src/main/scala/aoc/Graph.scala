package aoc

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._

object Graph {
  type Path[V] = NonEmptyList[V]
  type Paths[V] = List[Path[V]]
  type NeighborsFunction[V] = V => List[V]
  type VisitedFunction[V] = (Path[V], V) => Boolean
  object VisitedFunction {
    def fromEq[V: Eq]: VisitedFunction[V] = _.contains_(_)
  }

  // todo make tailrec
  def dfs[V](neighborsF: NeighborsFunction[V])(visitedF: VisitedFunction[V])(start: V): Paths[V] = {
    def it(path: Path[V], acc: Paths[V]): Paths[V] =
      neighborsF(path.head) match {
        case Nil       => path :: acc
        case neighbors => neighbors.filterNot(visitedF(path, _)).foldLeft(acc)((b, next) => it(next :: path, b))
      }
    it(NonEmptyList.one(start), Nil).map(_.reverse)
  }
}
