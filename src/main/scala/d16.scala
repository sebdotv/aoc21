import cats.Show
import cats.implicits._

import collection.mutable
import scala.annotation.tailrec

object d16 {
  // BITS transmission = packet
  // packet = packet packet ...
  // packet = standard_header
  // standard_header = version[3] type_ID[3]
  // type ID 4: literal value

//  sealed trait TypeID
//  object TypeID {
//    case object LiteralValue extends TypeID // 4
//  }

  type ParseResult[A] = (A, Bits)

  sealed trait PacketData
  object PacketData {
    def parse(typeID: Int, data: Bits): ParseResult[PacketData] =
      typeID match {
        case 4 => LiteralValuePacketData.parse(data)
        case _ => OperatorPacketData.parse(data)
      }
  }
  case class LiteralValuePacketData(value: Int) extends PacketData
  object LiteralValuePacketData {
    def parse(data: Bits): ParseResult[LiteralValuePacketData] = {
      @tailrec
      def it(bits: Bits, acc: Bits): ParseResult[Bits] = {
//        println(show"acc: $acc")
        val (keepReading :: group, remainder) = bits.splitAt(5)
//        println((keepReading, group, remainder))
        val updated = acc ++ group
        if (keepReading) it(remainder, updated) else (updated, remainder)
      }
      it(data, Nil).leftMap(bits => LiteralValuePacketData(bits.toDecimal))
    }
  }
  case class OperatorPacketData(subPackets: List[Packet]) extends PacketData
  object OperatorPacketData {
    def parse(data: Bits): ParseResult[OperatorPacketData] = {
//      val lengthTypeID :: rest = data
//      lengthTypeID match {
//        case false =>
//          val (h, subPackets) = rest.splitAt(15)
//          val totalLengthInBits = h.toDecimal
//        case true =>
//          val (h, subPackets) = rest.splitAt(11)
//          val numberOfSubPackets = h.toDecimal
//      }
      ???
    }
  }

  case class Packet(version: Int, typeID: Int, data: PacketData) {
    def toNode: Node =
      data match {
        case LiteralValuePacketData(value)  => LiteralValue(version = version, value = value)
        case OperatorPacketData(subPackets) => Operator(version = version, children = subPackets.map(_.toNode))
      }
  }
  object Packet {
    def parse(bits: Bits): ParseResult[Packet] = {
      val (header, data) = bits.splitAt(6)
      val (versionB, typeIDB) = header.splitAt(3)
      val typeID = typeIDB.toDecimal
      PacketData
        .parse(typeID, data)
        .leftMap(Packet(version = versionB.toDecimal, typeID = typeID, _))
    }
  }
  def parsePacket(input: String): Packet = {
    println(s"input: $input")
    val (packet, remainder) = Packet.parse(Bits.fromHex(input))
    println(s"packet: $packet")
    println(s"remainder: $remainder")
    assert(remainder.forall(_ === false))
    packet
  }
  def parseNode(line: String): Node =
    parsePacket(line).toNode

  sealed trait Node {
    def version: Int
    def children: List[Node]
    final def versionSum: Int =
      version + children.map(_.versionSum).sum
  }
  case class Operator(version: Int, children: List[Node]) extends Node

  case class LiteralValue(version: Int, value: Int) extends Node {
    override def children: List[Node] = Nil
  }

  type Bits = List[Boolean]
  implicit val showBits: Show[Bits] = _.toBinary
  implicit class BitsOps(bits: Bits) {
    def toBinary: String =
      bits.map(if (_) '1' else '0').mkString
    def toDecimal: Int =
      BigInt(toBinary, 2).toInt
  }
  object Bits {
    def fromBinary(s: String): Bits =
      s.map {
        case '0' => false
        case '1' => true
      }.toList
    def fromHex(s: String): Bits =
      fromBinary(BigInt(s, 16).toString(2))
  }
}
