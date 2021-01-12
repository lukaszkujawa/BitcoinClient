package io.bitcoinclient.bitcoin.messages

import io.bitcoinclient.utils.VariableInt
import io.bitcoinclient.utils.Endianess._

case class Addr(count: Int, ipAddresses: List[IpAddress]) extends Message {

  def toArray: Array[Byte] = Array()

}

case class IpAddress(timestamp: Int, services: Int, ip: Array[Byte], port: Int)

object Addr {

  /*
  Not a *very* functional function :/
   */
  def fromByteArray(raw: Array[Byte]) =  {
    val addrCount = VariableInt.parse(raw)
    var ips: List[IpAddress] = List()

    var offset = raw.head & 0xFF match {
      case 0xFF => 8
      case 0xFE => 4
      case 0xFD => 2
      case _ => 1
    }

    for( x <- 0 to addrCount - 1) {
      val pos = offset + 30 * x
      ips = ips.appended(IpAddress(
        raw.drop(pos).take(4).toIntFromLittleEndian,
        raw.drop(pos+4).take(4).toIntFromLittleEndian,
        raw.drop(pos+12).take(16),
        raw.drop(pos+28).take(2).toShortFromBigEndian & 0xFFFF,
      ))
    }

    Addr(addrCount, ips)
  }

}
