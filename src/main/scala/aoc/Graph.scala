package aoc

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._

object Graph {
  type Path[V] = NonEmptyList[V]
  type Paths[V] = List[Path[V]]
  type NeighborsFunction[V] = V => List[V]
  type WeightFunction[V, W] = (V, V) => W
  type VisitedFunction[V] = (Path[V], V) => Boolean
  type HeuristicFunction[V, W] = V => W
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

  // https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
  def aStar[V: Eq](
      neighborsF: NeighborsFunction[V]
  )(weightF: WeightFunction[V, Double])(heuristicF: HeuristicFunction[V, Double])(start: V, goal: V): Path[V] = {
    import collection.mutable

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    // todo switch impl
    val openSet = mutable.Set(start)

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    val cameFrom = mutable.Map.empty[V, V]

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    val gScore = mutable.Map.empty[V, Double].withDefaultValue(Double.PositiveInfinity)
    gScore(start) = 0

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n.
    val fScore = mutable.Map.empty[V, Double].withDefaultValue(Double.PositiveInfinity)
    fScore(start) = heuristicF(start)

    while (openSet.nonEmpty) {
      // This operation can occur in O(1) time if openSet is a min-heap or a priority queue
      val current = openSet.minBy(fScore) // the node in openSet having the lowest fScore[] value
      if (current === goal) {
        val total_path = mutable.ListBuffer(current)
        var c = current
        while (cameFrom.contains(c)) {
          c = cameFrom(c)
          total_path.prepend(c)
        }
        return NonEmptyList.fromListUnsafe(total_path.toList)
      } else {
        openSet.remove(current)
        for (neighbor <- neighborsF(current)) {
          // d(current,neighbor) is the weight of the edge from current to neighbor
          // tentative_gScore is the distance from start to the neighbor through current
          val tentative_gScore = gScore(current) + weightF(current, neighbor)
          if (tentative_gScore < gScore(neighbor)) {
            // This path to neighbor is better than any previous one. Record it!
            cameFrom(neighbor) = current
            gScore(neighbor) = tentative_gScore
            fScore(neighbor) = tentative_gScore + heuristicF(neighbor)
            if (!openSet.contains(neighbor)) {
              openSet.add(neighbor)
            }
          }
        }
      }
    }
    // Open set is empty but goal was never reached
    throw new RuntimeException("Failure")
  }
}
