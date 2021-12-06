object d06 {
  final case class SimState(indivs: Vector[Int]) {
    import SimState._
    def next: SimState = {
      copy(indivs = evolve(indivs))
    }
  }
  object SimState {
    def evolve(indivs: Vector[Int]): Vector[Int] = {
      val (updated, newIndivs) = indivs.map {
        case 0 => (6, List(8))
        case n => (n - 1, Nil)
      }.unzip
      updated ++ newIndivs.flatten
    }
  }
}
