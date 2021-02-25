package io.bitcoinclient.messages

import scodec.Attempt.Successful
import scodec._
import scodec.codecs._
import scodec.bits._


case class Version(version: Long,
                   services: Long,
                   timestamp: Long,
                   addr_recv_services: Long,
                   addr_recv_ipv6: ByteVector,
                   addr_recv_port: Short,
                   addr_trans_services: Long,
                   addr_trans_ipv6: ByteVector,
                   addr_trans_port: Short,
                   nounce: ByteVector,
                   agent: String,
                   start_height: Long,
                   relay: Boolean) extends Message {

  def toEither: Either[Err, Array[Byte]] = Codec[Version].encode(this).map(_.toByteArray).toEither

}


object Version {

  val commandName = "version"

  implicit val codec: Codec[Version] = {
    ("version" | uint32L) ::
    ("services" | (uint32L.dropRight(ignore(32)))) ::
    ("timestamp" | (uint32L.dropRight(ignore(32))) ) ::
    ("addr_recv_services" | (uint32L.dropRight(ignore(32))) ) ::
    ("addr_recv_ipv6" | bytes(16)) ::
    ("addr_recv_port" | short16L) ::
    ("addr_trans_services" | (uint32L.dropRight(ignore(32))) ) ::
    ("addr_trans_ipv6" | bytes(16)) ::
    ("addr_trans_port" | short16L) ::
    ("nounce" | bytes(8)) ::
    ("agent" | variableSizeBytes(uint8, ascii) ) ::
    ("start_height" | uint32L ) ::
    ("relay" | bool(8))
  }.as[Version]

  def fromByteArray(data: Array[Byte]): Either[Any, Version] =
    Codec[Version].decode(BitVector(data)).map(_.value).toEither
}
