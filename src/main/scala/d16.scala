import cats.Show
import cats.implicits._

import scala.annotation.tailrec

object d16 {
  type ParseResult[A] = (A, Bits)

  sealed trait PacketData
  object PacketData {
    def parse(typeID: Int, data: Bits): ParseResult[PacketData] =
      typeID match {
        case 4 => LiteralValuePacketData.parse(data)
        case _ => OperatorPacketData.parse(data)
      }
  }

  case class LiteralValuePacketData(value: Long) extends PacketData
  object LiteralValuePacketData {
    def parse(data: Bits): ParseResult[LiteralValuePacketData] = {
      @tailrec
      def it(bits: Bits, acc: Bits): ParseResult[Bits] = {
        val (keepReading :: group, remainder) = bits.splitAt(5)
        val updated = acc ++ group
        if (keepReading) it(remainder, updated) else (updated, remainder)
      }
      it(data, Nil).leftMap(bits => LiteralValuePacketData(bits.toDecimal))
    }
  }

  case class OperatorPacketData(subPackets: List[Packet]) extends PacketData
  object OperatorPacketData {
    def parse(data: Bits): ParseResult[OperatorPacketData] = {
      val lengthTypeID :: rest = data
      val parseResult = if (!lengthTypeID) {
        val (h, subPackets) = rest.splitAt(15)
        val totalLengthInBits = h.toDecimal.toInt
        @tailrec
        def it(remainingBits: Int, remainder: Bits, acc: List[Packet]): ParseResult[List[Packet]] = {
          if (remainingBits === 0) (acc, remainder)
          else {
            val (packet, updatedRemainder) = Packet.parse(remainder)
            it(remainingBits - (remainder.size - updatedRemainder.size), updatedRemainder, packet :: acc)
          }
        }
        it(totalLengthInBits, subPackets, Nil)
      } else {
        val (h, subPackets) = rest.splitAt(11)
        val numberOfSubPackets = h.toDecimal.toInt
        @tailrec
        def it(n: Int, remainder: Bits, acc: List[Packet]): ParseResult[List[Packet]] =
          if (n === 0) (acc, remainder)
          else {
            val (packet, updatedRemainder) = Packet.parse(remainder)
            it(n - 1, updatedRemainder, packet :: acc)
          }
        it(numberOfSubPackets, subPackets, Nil)
      }
      parseResult.leftMap(subPackets => OperatorPacketData(subPackets.reverse))
    }
  }

  case class Packet(version: Int, typeID: Int, data: PacketData) {
    def toNode: Node =
      data match {
        case LiteralValuePacketData(value)  => LiteralValue(version = version, value = value)
        case OperatorPacketData(subPackets) => Operator(version = version, children = subPackets.map(_.toNode))
      }

    def value: Long = data match {
      case LiteralValuePacketData(value) => value
      case OperatorPacketData(subPackets) =>
        val values = subPackets.map(_.value)
        typeID match {
          case 0 => values.sum
          case 1 => values.product
          case 2 => values.min
          case 3 => values.max
          case 5 =>
            val List(a, b) = values
            if (a > b) 1 else 0
          case 6 =>
            val List(a, b) = values
            if (a < b) 1 else 0
          case 7 =>
            val List(a, b) = values
            if (a === b) 1 else 0
        }
    }
  }
  object Packet {
    def parse(bits: Bits): ParseResult[Packet] = {
      val (header, data) = bits.splitAt(6)
      val (versionB, typeIDB) = header.splitAt(3)
      val typeID = typeIDB.toDecimal.toInt
      PacketData
        .parse(typeID, data)
        .leftMap(Packet(version = versionB.toDecimal.toInt, typeID = typeID, _))
    }
  }

  def parse(input: String): Packet = {
    val (packet, remainder) = Packet.parse(Bits.fromHex(input))
    assert(remainder.forall(_ === false))
    packet
  }
  def parseAsNode(line: String): Node =
    parse(line).toNode

  sealed trait Node {
    def version: Int
    def children: List[Node]
    final def versionSum: Int =
      version + children.map(_.versionSum).sum
  }
  case class Operator(version: Int, children: List[Node]) extends Node
  case class LiteralValue(version: Int, value: Long) extends Node {
    override def children: List[Node] = Nil
  }

  type Bits = List[Boolean]
  implicit val showBits: Show[Bits] = _.toBinary
  implicit class BitsOps(bits: Bits) {
    def toBinary: String =
      bits.map(if (_) '1' else '0').mkString
    def toDecimal: Long =
      BigInt(toBinary, 2).toLong
  }
  object Bits {
    def fromBinary(s: String): Bits =
      s.map {
        case '0' => false
        case '1' => true
      }.toList
    def fromHex(s: String): Bits = {
      val raw = BigInt(s, 16).toString(2)
      val padding = s.length * 4 - raw.length
      val leftPadded = ("0" * padding) + raw
      fromBinary(leftPadded)
    }
  }
}
