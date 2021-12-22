import cats.implicits._

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
    def decode(data: BitString): LiteralValue =
      ???
  }

  case class Packet(version: Int, typeID: Int, data: BitString) {
    def decode: DecodedPacket = {
      typeID match {
        case 4 => LiteralValue.decode(data)
      }
    }
  }
  object Packet {
    def from(bs: BitString): Packet = {
      println(bs)
      val (header, data) = bs.splitAt(6)
      println(header)
      val (version, typeID) = header.splitAt(3)
      println(version)
      println(typeID)
      Packet(version = version.toDecimal, typeID = typeID.toDecimal, data = data)
    }
  }
  def decode(line: String): DecodedPacket = {
    rawDecode(line).decode
  }

  case class BitString(bits: List[Boolean]) {
    def splitAt(n: Int): (BitString, BitString) = {
      val (a, b) = bits.splitAt(n)
      (BitString(a), BitString(b))
    }

    def toBinary: String = bits.map(if (_) '1' else '0').mkString
    def toDecimal: Int = BigInt(toBinary, 2).toInt
  }
  object BitString {
    def fromBinary(s: String): BitString =
      BitString(s.map {
        case '0' => false
        case '1' => true
      }.toList)
    def fromHex(s: String): BitString =
      fromBinary(BigInt(s, 16).toString(2))
  }

  def rawDecode(line: String): Packet = {
    Packet.from(BitString.fromHex(line))
  }
}
