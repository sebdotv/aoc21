import aoc._

object d18 {

  sealed trait SnailfishNumber
  case class RegularNumber(value: Int) extends SnailfishNumber
  case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber

  sealed trait BidiSnailfishNumber
  case class MutableBidiRegularNumber(parent: Option[MutableBidiPair], var value: Int) extends BidiSnailfishNumber {
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

    override def toString: String =
      s"MutableBidiPair(${if (parent.isEmpty) "None" else "Some(_)"}, $left, $right)"
  }

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

  def toBidi(parent: Option[MutableBidiPair], n: SnailfishNumber): BidiSnailfishNumber =
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
  // Exploding pairs will always consist of two regular numbers. Then, the entire exploding pair is replaced with the regular number 0.
  def explode(pair: MutableBidiPair): Unit = {
    val left = pair.left match {
      case MutableBidiRegularNumber(_, value) => value
    }
    val right = pair.right match {
      case MutableBidiRegularNumber(_, value) => value
    }

  }

}
