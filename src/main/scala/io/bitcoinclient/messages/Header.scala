package io.bitcoinclient.messages

import io.bitcoinclient.bitcoin.messages.Header.sha256
import scodec._
import scodec.codecs._
import scodec.bits._


case class Header(magicBytes: Unit, commandName: String, payloadSize: Long, checkSum: ByteVector) extends Message {

  def toEither: Either[Err, Array[Byte]] = Codec[Header].encode(this).map(_.toByteArray).toEither

}

object Header {

  val SIZE = 24

  val commandNameCodec = bytes(12).exmap(
    bv => ascii.decodeValue(bv.bits),
    (s: String) => Attempt.successful(ByteVector.apply(s.getBytes))
  )

  implicit val codec: Codec[Header] = {
      ("magicBytes" | constant(hex"f9beb4d9")) ::
      ("commandName" | commandNameCodec) ::
      ("payloadSize" | uint32L) ::
      ("checkSum" | bytes(4))
  }.as[Header]

  def checksum(data: Array[Byte]): ByteVector =
      ByteVector.apply(sha256.digest(sha256.digest(data)).take(4))

  def fromByteArray(data: Array[Byte]): Either[Any, Header] =
      Codec[Header].decode(BitVector(data)).map(_.value).toEither
}
