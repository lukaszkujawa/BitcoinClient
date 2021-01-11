package io.bitcoinclient.bitcoin.messages

import java.io.{ByteArrayOutputStream, DataOutputStream}

import io.bitcoinclient.utils.Eindianess._

case class Version(version: Array[Byte],
                   services: Array[Byte],
                   timestamp: Int,
                   addr_recv_services: Array[Byte],
                   addr_recv_ipv6: Array[Byte],
                   addr_recv_port: Short,
                   addr_trans_services: Array[Byte],
                   addr_trans_ipv6: Array[Byte],
                   addr_trans_port: Short,
                   nounce: Array[Byte],
                   agent: String,
                   start_height: Int,
                   relay: Byte
                  ) extends Message {


  val byteOutputStream = new ByteArrayOutputStream()
  val rawVersion = new DataOutputStream(byteOutputStream)

  def updateAndGet: Array[Byte] = {


    rawVersion.write(version)
    rawVersion.write(services)
    rawVersion.write(timestamp.toLittleEndian)
    rawVersion.writeInt(0)
    rawVersion.write(addr_recv_services)
    rawVersion.write(addr_recv_ipv6)
    rawVersion.write(addr_recv_port.toBigEndian)
    rawVersion.write(addr_trans_services)
    rawVersion.write(addr_trans_ipv6)
    rawVersion.write(addr_recv_port.toBigEndian)
    rawVersion.write(nounce)
    rawVersion.write(Array(agent.length.toByte) ++ agent.getBytes())
    rawVersion.write(start_height.toLittleEndian)
    rawVersion.write(relay)

    rawVersion.flush()

    byteOutputStream.toByteArray
  }

  def toArray: Array[Byte] = byteOutputStream.size match {
    case 0 => updateAndGet
    case _ => byteOutputStream.toByteArray
  }

}

object Version {

  def create(version: Int,
             services: Int,
             trans_ipv6: Array[Byte],
             trans_port: Short,
             nounce: String,
             agent: String,
             start_height: Int): Option[Version] = {


    ( trans_ipv6.length == 16 && nounce.length == 8) match {
      case true => Some(Version(
        version.toLittleEndian,
        services.toLittleEndian ++ 0.toLittleEndian,
        (System.currentTimeMillis / 1000).toInt,
        services.toLittleEndian ++ 0.toLittleEndian,
        Array[Byte](0,0,0,0,0,0,0,0,0,0,0xFF.toByte,0xFF.toByte,0x7F.toByte,0,0,0x01),
        8333,
        services.toLittleEndian ++ 0.toLittleEndian,
        trans_ipv6,
        trans_port,
        nounce.getBytes,
        agent,
        start_height,
        1
      ))
      case _ => None
    }

  }

  def fromByteArray(raw: Array[Byte]) =  {

    Version(
      raw.take(4),
      raw.drop(4).take(8),
      raw.drop(12).take(4).toIntFromLittleEndian,
      raw.drop(20).take(8),
      raw.drop(28).take(16),
      raw.drop(44).take(2).toShortFromBigEndian,
      raw.drop(46).take(8),
      raw.drop(54).take(16),
      raw.drop(70).take(2).toShortFromBigEndian,
      raw.drop(72).take(8),
      raw.drop(81).take(raw.drop(80).head.toInt).map(_.toChar).mkString,
      raw.drop(81 + raw.drop(80).head.toInt).toIntFromLittleEndian,
      0)
  }

}
