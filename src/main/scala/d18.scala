import aoc._

object d18 {

  sealed trait SnailfishNumber
  case class RegularNumber(value: Int) extends SnailfishNumber
  case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber

  def parse(s: String): SnailfishNumber = {
    val NumberRe = """^(\d+)(.*)""".r
    type ParseResult = (SnailfishNumber, String)
    def parseInternal(s: String)(indentLevel: Int): ParseResult = {
      def log(msg: Any): Unit = println(s"${"  ".repeat(indentLevel)}$msg")
      val result = s.splitAt(1) match {
        case ("[", rest) =>
          log(s"parsing left from $rest")
          val (left, rest1) = parseInternal(rest)(indentLevel + 1)
          val (",", rest2) = rest1.splitAt(1)
          log(s"parsing right from $rest2")
          val (right, rest3) = parseInternal(rest2)(indentLevel + 1)
          val ("]", rest4) = rest3.splitAt(1)
          val pair = Pair(left, right)
          log(s"pair found: $pair")
          (pair, rest4)
        case _ =>
          val NumberRe(number, rest) = s
          (RegularNumber(number.toInt), rest)
      }
      log(result)
      result
    }
    println(s"parse($s)")
    val (element, "") = parseInternal(s)(1)
    println(s"=> $element")
    element
  }

//  def toMutableContainer(parent: Option[MutableContainer], value: SnailfishNumber): MutableContainer =
//    value match {
//      case n @ RegularNumber(_) => MutableContainer(parent, n)
//      case Pair(left, right)    => MutableContainer(parent, )
//    }

  sealed trait BidiSnailfishNumber {
    def parent: Option[MutableBidiPair]
    def firstLeaf: Option[MutableBidiRegularNumber]
    def lastLeaf: Option[MutableBidiRegularNumber]

    def toText: String

    def replaceInParent(other: SnailfishNumber): Unit = {
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) {
            parentPair.leftOption = Some(toBidi(Some(parentPair), other))
          } else if (this == parentPair.right) {
            parentPair.rightOption = Some(toBidi(Some(parentPair), other))
          } else {
            throw new IllegalStateException()
          }
        case None => throw new IllegalStateException()
      }
    }
  }

  case class MutableBidiRegularNumber(parent: Option[MutableBidiPair], var value: Int) extends BidiSnailfishNumber {
    override def firstLeaf: Option[MutableBidiRegularNumber] = Some(this)
    override def lastLeaf: Option[MutableBidiRegularNumber] = Some(this)
    def add(x: Int): Unit = value += x

    override def toText: String = value.toString
    override def toString: String =
      s"MutableBidiRegularNumber(${if (parent.isEmpty) "None" else "Some(_)"}, $value)"
  }
  class MutableBidiPair(
      val parent: Option[MutableBidiPair],
      var leftOption: Option[BidiSnailfishNumber] = None,
      var rightOption: Option[BidiSnailfishNumber] = None
  ) extends BidiSnailfishNumber {
    def left: BidiSnailfishNumber = leftOption.unsafeGet()
    def right: BidiSnailfishNumber = rightOption.unsafeGet()

    override def firstLeaf: Option[MutableBidiRegularNumber] = left.firstLeaf
    override def lastLeaf: Option[MutableBidiRegularNumber] = right.lastLeaf

    def prevLeaf: Option[MutableBidiRegularNumber] =
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) {
            parentPair.prevLeaf
          } else if (this == parentPair.right) {
            parentPair.left.lastLeaf
          } else {
            throw new IllegalStateException()
          }
        case None => None
      }

    def nextLeaf: Option[MutableBidiRegularNumber] =
      parent match {
        case Some(parentPair) =>
          if (this == parentPair.left) {
            parentPair.right.firstLeaf
          } else if (this == parentPair.right) {
            parentPair.nextLeaf
          } else {
            throw new IllegalStateException()
          }
        case None => None
      }

    override def toText: String = s"[${left.toText},${right.toText}]"
    override def toString: String =
      s"MutableBidiPair(${if (parent.isEmpty) "None" else "Some(_)"}, $left, $right)"
  }

  implicit class MutableBidiPairOps(pair: MutableBidiPair) {
    def leftPairUnsafe(): MutableBidiPair = pair.left.asInstanceOf[MutableBidiPair]
    def rightPairUnsafe(): MutableBidiPair = pair.right.asInstanceOf[MutableBidiPair]
    def leftNumberUnsafe(): MutableBidiRegularNumber = pair.left.asInstanceOf[MutableBidiRegularNumber]
    def rightNumberUnsafe(): MutableBidiRegularNumber = pair.right.asInstanceOf[MutableBidiRegularNumber]
  }

  def toBidi(n: SnailfishNumber): BidiSnailfishNumber =
    toBidi(None, n)
  private def toBidi(parent: Option[MutableBidiPair], n: SnailfishNumber): BidiSnailfishNumber =
    n match {
      case RegularNumber(value) =>
        MutableBidiRegularNumber(parent, value)
      case Pair(left, right) =>
        val pair = new MutableBidiPair(parent)
        pair.leftOption = Some(toBidi(Some(pair), left))
        pair.rightOption = Some(toBidi(Some(pair), right))
        pair
    }

  // To explode a pair, the pair's left value is added to the first regular number to the left of the exploding pair (if any),
  // and the pair's right value is added to the first regular number to the right of the exploding pair (if any).
  // Exploding pairs will always consist of two regular numbers.
  // Then, the entire exploding pair is replaced with the regular number 0.
  def explode(pair: MutableBidiPair): Unit = {
    pair.prevLeaf.foreach(_.add(pair.leftNumberUnsafe().value))
    pair.nextLeaf.foreach(_.add(pair.rightNumberUnsafe().value))
    pair.replaceInParent(RegularNumber(0))
  }

  // To split a regular number, replace it with a pair;
  // the left element of the pair should be the regular number divided by two and rounded down,
  // while the right element of the pair should be the regular number divided by two and rounded up.
  def split(rn: MutableBidiRegularNumber): Unit =
    rn.replaceInParent(Pair(RegularNumber(rn.value / 2), RegularNumber((rn.value + 1) / 2)))

}
