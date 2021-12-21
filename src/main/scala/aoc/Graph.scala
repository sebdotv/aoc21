package aoc

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import org.jgrapht.alg.interfaces.AStarAdmissibleHeuristic
import org.jgrapht.alg.shortestpath.AStarShortestPath
import org.jgrapht.graph.{DefaultWeightedEdge, SimpleDirectedWeightedGraph}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object Graph {
  type Path[V] = NonEmptyList[V]
  type Paths[V] = List[Path[V]]
  type NeighborsFunction[V] = V => List[V]
  type WeightFunction[V, W] = (V, V) => W
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

  // https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode
  def aStarSlow[V: Eq](
      neighborsF: NeighborsFunction[V]
  )(weightF: WeightFunction[V, Double])(heuristicF: WeightFunction[V, Double])(start: V, goal: V): Path[V] = {
    import collection.mutable

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    // todo slow, must switch impl (PriorityQueue was tried, but remove/update is too slow)
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
    fScore(start) = heuristicF(start, goal)

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
            fScore(neighbor) = tentative_gScore + heuristicF(neighbor, goal)
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

  def aStar[V: Eq](
      neighborsF: NeighborsFunction[V]
  )(weightF: WeightFunction[V, Double])(heuristicF: WeightFunction[V, Double])(vertices: List[V], start: V, goal: V): Path[V] = {
    val graph = new SimpleDirectedWeightedGraph[V, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    for (v <- vertices) {
      graph.addVertex(v)
    }
    for (v <- vertices) {
      for (n <- neighborsF(v)) {
        graph.addEdge(v, n)
        graph.setEdgeWeight(v, n, weightF(v, n))
      }
    }
    val admissibleHeuristic = new AStarAdmissibleHeuristic[V] {
      override def getCostEstimate(sourceVertex: V, targetVertex: V): Double =
        heuristicF(sourceVertex, targetVertex)
    }
    val algo = new AStarShortestPath(graph, admissibleHeuristic)
    val path = algo.getPath(start, goal)
    NonEmptyList.fromListUnsafe(path.getVertexList.asScala.toList)
  }

  def dijkstra[V](neighborsF: NeighborsFunction[V])(lengthF: WeightFunction[V, Int])(vertices: List[V], source: V) = {
    import collection.mutable

    val dist = mutable.Map.empty[V, Int]
    val prev = mutable.Map.empty[V, Option[V]]
//    implicit val orderingV: Ordering[V] = Ordering.fromLessThan()
//    val pq = mutable.PriorityQueue.empty[V]
    val q = mutable.Set.empty[V]

    vertices.foreach { v =>
      dist(v) = Int.MaxValue
      prev(v) = None
      q.add(v)
    }
    dist(source) = 0

    while (q.nonEmpty) {
      val u = q.minBy(dist)
      q.remove(u)
      for (v <- neighborsF(u) if q.contains(v)) {
        val alt = if (dist(u) === Int.MaxValue) Int.MaxValue else (dist(u) + lengthF(u, v))
        if (alt < dist(v)) {
          dist(v) = alt
          prev(v) = Some(u)
        }
      }
    }

    (dist.toMap, prev.toMap)

    // 1  function Dijkstra(Graph, source):
    // 2
    // 3      create vertex set Q
    // 4
    // 5      for each vertex v in Graph:
    // 6          dist[v] ← INFINITY
    // 7          prev[v] ← UNDEFINED
    // 8          add v to Q
    // 9      dist[source] ← 0
    // 10
    // 11      while Q is not empty:
    // 12          u ← vertex in Q with min dist[u]
    // 13
    // 14          remove u from Q
    // 15
    // 16          for each neighbor v of u still in Q:
    // 17              alt ← dist[u] + length(u, v)
    // 18              if alt < dist[v]:
    // 19                  dist[v] ← alt
    // 20                  prev[v] ← u
    // 21
    // 22      return dist[], prev[]
  }
}
