import cats.implicits._
import collection.mutable

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

  sealed trait DecodedPacket
  case class LiteralValue(value: Int) extends DecodedPacket
  object LiteralValue {
    def decode(data: Bits): LiteralValue = {
      val groups = data.grouped(5)
      val buffer = mutable.ListBuffer.empty[Boolean]
      var lastGroupSeen = false
      for (group <- groups) {
        if (!lastGroupSeen) {
          val h :: rest = group
          buffer ++= rest
          lastGroupSeen = !h
        }
      }
      LiteralValue(buffer.toList.toDecimal)
    }
  }

  case class Packet(version: Int, typeID: Int, data: Bits) {
    def decode: DecodedPacket = {
      typeID match {
        case 4 => LiteralValue.decode(data)
      }
    }
  }
  object Packet {
    def from(bits: Bits): Packet = {
      val (header, data) = bits.splitAt(6)
      val (version, typeID) = header.splitAt(3)
      Packet(version = version.toDecimal, typeID = typeID.toDecimal, data = data)
    }
  }
  def decode(line: String): DecodedPacket =
    rawDecode(line).decode

  def rawDecode(line: String): Packet = {
    Packet.from(Bits.fromHex(line))
  }

  type Bits = List[Boolean]
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
