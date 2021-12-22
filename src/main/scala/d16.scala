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
      val lengthTypeID :: rest = data
      println(show"lengthTypeID=$lengthTypeID, rest=$rest")
      val parseResult = lengthTypeID match {
        case false =>
          val (h, subPackets) = rest.splitAt(15)
          println(show"h=$h subPackets=$subPackets")
          val totalLengthInBits = h.toDecimal
          println(show"totalLengthInBits=$totalLengthInBits, subPackets=$subPackets")
          @tailrec
          def it(remainingBits: Int, remainder: Bits, acc: List[Packet]): ParseResult[List[Packet]] = {
            println(show"it($remainingBits, $remainder, ${acc.toString})")
            if (remainingBits === 0) (acc, remainder)
            else {
              val (packet, updatedRemainder) = Packet.parse(remainder)
              it(remainingBits - (remainder.size - updatedRemainder.size), updatedRemainder, packet :: acc)
            }
          }
          it(totalLengthInBits, subPackets, Nil)
        case true =>
          val (h, subPackets) = rest.splitAt(11)
          val numberOfSubPackets = h.toDecimal
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
  }
  object Packet {
    def parse(bits: Bits): ParseResult[Packet] = {
      println(show"Packet.parse($bits)")
      val (header, data) = bits.splitAt(6)
      val (versionB, typeIDB) = header.splitAt(3)
      println(show"versionB=$versionB, typeIDB=$typeIDB, data=$data")
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
    def fromHex(s: String): Bits = {
      val raw = BigInt(s, 16).toString(2)
      val modulo = raw.length % 4
      val padding = (4 - modulo) % 4
      val leftPadded = ("0" * padding) + raw
      fromBinary(leftPadded)
    }
  }
}
