package io.bitcoinclient.bitcoin.messages

import io.bitcoinclient.utils.VariableInt
import io.bitcoinclient.utils.Endianess._
import java.net.InetAddress
import java.net.Inet6Address

case class Addr(count: Int, ipAddresses: List[IpAddress]) extends Message {

  def toArray: Array[Byte] = Array()

  override def toString: String = ipAddresses.foldLeft("")((acc,ip) => acc + ip + "\n")

}

case class IpAddress(timestamp: Int, services: Int, ip: Array[Byte], port: Int) {

  val ipv4Mapping : Array[Byte] = Array(0,0,0,0,0,0,0,0,0,0,0xff.toByte,0xff.toByte)

  def getIp: String =
    ip.take(10).sum == 0 && ip.take(12).sum == -2 match {
      case true => (ip.drop(12).head & 0xFF) + "." + (ip.drop(13).head & 0xFF) + "." + (ip.drop(14).head & 0xFF) + "." + (ip.drop(15).head & 0xFF)
      case _ => InetAddress.getByName(ip.map("%02X" format _).zipWithIndex.foldLeft("")((acc,v) => acc + v._1 + (v._2 % 2 == 1 match {
        case true => ":"
        case _ => ""
      })).dropRight(1)).getHostName
    }

  override def toString: String = getIp.contains(':') match {
    case true => "[" + getIp + "]:" + port
    case _ => getIp + ":" + port
  }



}

object IpAddress {

  def fromByteArray(raw: Array[Byte]): IpAddress = IpAddress(
    raw.take(4).toIntFromLittleEndian,
    raw.drop(4).take(4).toIntFromLittleEndian,
    raw.drop(12).take(16),
    raw.drop(28).take(2).toShortFromBigEndian & 0xFFFF,
  )

}


object Addr {

  def fromByteArray(raw: Array[Byte]) =  {
    val addrCount = VariableInt.parse(raw)

    var offset = raw.head & 0xFF match {
      case 0xFF => 9
      case 0xFE => 5
      case 0xFD => 3
      case _ => 1
    }

    var ips: List[IpAddress] = raw.drop(offset).grouped(30).map(rawIp => IpAddress.fromByteArray(rawIp)).toList

    Addr(addrCount, ips)
  }

  /*
  Not a *very* functional function :/
   */

  /*
  def fromByteArray(raw: Array[Byte]) =  {
    val addrCount = VariableInt.parse(raw)
    var ips: List[IpAddress] = List()

    var offset = raw.head & 0xFF match {
      case 0xFF => 9
      case 0xFE => 5
      case 0xFD => 3
      case _ => 1
    }

    for( x <- 0 to addrCount - 1) {
      val pos = offset + (30 * x)
      ips = ips.appended(IpAddress(
        raw.drop(pos).take(4).toIntFromLittleEndian,
        raw.drop(pos+4).take(4).toIntFromLittleEndian,
        raw.drop(pos+12).take(16),
        raw.drop(pos+28).take(2).toShortFromBigEndian & 0xFFFF,
      ))
    }

    Addr(addrCount, ips)
  }

   */

}
