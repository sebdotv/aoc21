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
}
