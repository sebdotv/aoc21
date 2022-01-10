import aoc._
import cats._
import cats.implicits._

import scala.annotation.tailrec

object d18 {

  sealed trait SnailfishNumber
  case class RegularNumber(value: Int) extends SnailfishNumber
  case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber
  implicit val showSnailfishNumber: Show[SnailfishNumber] = {
    case RegularNumber(value) => value.show
    case Pair(left, right)    => show"[$left,$right]"
  }
  implicit class SnailfishNumberOps(n: SnailfishNumber) {
    def magnitude: Int =
      n match {
        case RegularNumber(value) => value
        case Pair(left, right)    =>
          // The magnitude of a pair is 3 times the magnitude of its left element
          // plus 2 times the magnitude of its right element
          3 * left.magnitude + 2 * right.magnitude
      }
  }

  def parse(s: String): SnailfishNumber = {
    val NumberRe = """^(\d+)(.*)""".r
    def parseInternal(s: String)(indentLevel: Int): (SnailfishNumber, String) =
      s.splitAt(1) match {
        case ("[", rest) =>
          val (left, rest1) = parseInternal(rest)(indentLevel + 1)
          val (",", rest2) = rest1.splitAt(1)
          val (right, rest3) = parseInternal(rest2)(indentLevel + 1)
          val ("]", rest4) = rest3.splitAt(1)
          val pair = Pair(left, right)
          (pair, rest4)
        case _ =>
          val NumberRe(number, rest) = s
          (RegularNumber(number.toInt), rest)
      }
    val (element, "") = parseInternal(s)(1)
    element
  }

  sealed trait MutableNode {
    def parent: Option[MutablePairNode]
    def firstLeaf: Option[MutableRegularNumberNode]
    def lastLeaf: Option[MutableRegularNumberNode]

    def toModel: SnailfishNumber
    final def toText: String = toModel.show

    def replaceInParent(other: SnailfishNumber): Unit = {
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) {
            parentPair.leftOption = Some(toMutable(Some(parentPair), other))
          } else if (this == parentPair.right) {
            parentPair.rightOption = Some(toMutable(Some(parentPair), other))
          } else {
            throw new IllegalStateException()
          }
        case None => throw new IllegalStateException()
      }
    }

    def allNodes: List[MutableNode]

    def level: Int = parent match {
      case Some(parent) => 1 + parent.level
      case None         => 0
    }
  }

  case class MutableRegularNumberNode(parent: Option[MutablePairNode], var value: Int) extends MutableNode {
    override def firstLeaf: Option[MutableRegularNumberNode] = Some(this)
    override def lastLeaf: Option[MutableRegularNumberNode] = Some(this)

    override def allNodes: List[MutableNode] = List(this)

    def add(x: Int): Unit = value += x

    override def toModel: SnailfishNumber = RegularNumber(value)

    override def toString: String =
      s"MutableRegularNumberNode(${if (parent.isEmpty) "None" else "Some(_)"}, $value)"
  }
  class MutablePairNode(
      val parent: Option[MutablePairNode],
      var leftOption: Option[MutableNode] = None,
      var rightOption: Option[MutableNode] = None
  ) extends MutableNode {
    def left: MutableNode = leftOption.unsafeGet()
    def right: MutableNode = rightOption.unsafeGet()

    override def allNodes: List[MutableNode] =
      left.allNodes ++ right.allNodes ++ List(this)

    override def firstLeaf: Option[MutableRegularNumberNode] = left.firstLeaf
    override def lastLeaf: Option[MutableRegularNumberNode] = right.lastLeaf

    def prevLeaf: Option[MutableRegularNumberNode] =
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) parentPair.prevLeaf
          else if (this == parentPair.right) parentPair.left.lastLeaf
          else throw new IllegalStateException()
        case None => None
      }

    def nextLeaf: Option[MutableRegularNumberNode] =
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) parentPair.right.firstLeaf
          else if (this == parentPair.right) parentPair.nextLeaf
          else throw new IllegalStateException()
        case None => None
      }

    override def toModel: SnailfishNumber = Pair(left.toModel, right.toModel)

    override def toString: String =
      s"MutablePairNode(${if (parent.isEmpty) "None" else "Some(_)"}, $left, $right)"
  }

  implicit class MutablePairNodeOps(pair: MutablePairNode) {
    def leftPairUnsafe(): MutablePairNode = pair.left.asInstanceOf[MutablePairNode]
    def rightPairUnsafe(): MutablePairNode = pair.right.asInstanceOf[MutablePairNode]
    def leftNumberUnsafe(): MutableRegularNumberNode = pair.left.asInstanceOf[MutableRegularNumberNode]
    def rightNumberUnsafe(): MutableRegularNumberNode = pair.right.asInstanceOf[MutableRegularNumberNode]
  }

  def toMutable(n: SnailfishNumber): MutableNode =
    toMutable(None, n)
  private def toMutable(parent: Option[MutablePairNode], n: SnailfishNumber): MutableNode =
    n match {
      case RegularNumber(value) =>
        MutableRegularNumberNode(parent, value)
      case Pair(left, right) =>
        val pair = new MutablePairNode(parent)
        pair.leftOption = Some(toMutable(Some(pair), left))
        pair.rightOption = Some(toMutable(Some(pair), right))
        pair
    }

  // To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any),
  // and the pair's right value is added to the first regular number to the right of the exploding pair (if any).
  // Exploding pairs will always consist of two regular numbers.
  // Then, the entire exploding pair is replaced with the regular number 0.
  def explode(pair: MutablePairNode): Unit = {
    pair.prevLeaf.foreach(_.add(pair.leftNumberUnsafe().value))
    pair.nextLeaf.foreach(_.add(pair.rightNumberUnsafe().value))
    pair.replaceInParent(RegularNumber(0))
  }

  // To split a regular number, replace it with a pair;
  // the left element of the pair should be the regular number divided by two and rounded down,
  // while the right element of the pair should be the regular number divided by two and rounded up.
  def split(rn: MutableRegularNumberNode): Unit =
    rn.replaceInParent(Pair(RegularNumber(rn.value / 2), RegularNumber((rn.value + 1) / 2)))

  private def reduceStep(root: MutableNode): Option[Unit] = {
    val nodes = root.allNodes
    nodes
      .collectFirst { case pair: MutablePairNode if pair.level >= 4 => pair }
      .map(explode)
      .orElse {
        nodes
          .collectFirst { case rn: MutableRegularNumberNode if rn.value >= 10 => rn }
          .map(split)
      }
  }

  @tailrec
  private def reduce(root: MutableNode): Unit = {
    reduceStep(root) match {
      case Some(()) => reduce(root)
      case None     => ()
    }
  }

  def add(a: SnailfishNumber, b: SnailfishNumber): SnailfishNumber = {
    val root = toMutable(Pair(a, b))
    reduce(root)
    root.toModel
  }

  def finalSum(numbers: List[SnailfishNumber]): SnailfishNumber = {
    val a :: b :: rest = numbers
    rest.foldLeft(add(a, b))((acc, curr) => add(acc, curr))
  }

  def part2(numbers: List[SnailfishNumber]): Int = {
    val sums = for ((a, ia) <- numbers.zipWithIndex; (b, ib) <- numbers.zipWithIndex if ia =!= ib) yield add(a, b)
    sums.map(_.magnitude).max
  }
}
