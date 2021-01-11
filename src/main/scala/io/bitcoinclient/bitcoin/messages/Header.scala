package io.bitcoinclient.bitcoin.messages

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.security.MessageDigest
import java.nio.ByteBuffer
import io.bitcoinclient.utils.Eindianess._

case class Header(magicBytes: Int, commandName: Array[Byte], payloadSize: Int, checkSum: Array[Byte]) extends  Message {

  val byteOutputStream = new ByteArrayOutputStream()
  val rawHeader = new DataOutputStream(byteOutputStream)

  def updateAndGet: Array[Byte] = {
    rawHeader.writeInt(magicBytes)
    rawHeader.write(commandName)
    rawHeader.write(payloadSize.toLittleEndian)
    rawHeader.write(checkSum)

    rawHeader.flush()

    byteOutputStream.toByteArray
  }

  def toArray: Array[Byte] = byteOutputStream.size match {
    case 0 => updateAndGet
    case _ => byteOutputStream.toByteArray
  }

}


object Header {

  val magic: Int = 0xf9beb4d9
  val sha256 = MessageDigest.getInstance("SHA-256")

  def getMessageName(m: Message): Array[Byte] = m match {
    case _: Version => Array('v', 'e', 'r', 's', 'i', 'o', 'n', 0, 0, 0, 0, 0)
    case _: VerAck => Array('v', 'e', 'r', 'a', 'c', 'k', 0, 0, 0, 0, 0, 0)
    case _: GetAddr => Array('g', 'e', 't', 'a', 'd', 'd', 'r', 0, 0, 0, 0, 0)
    case _: Addr => Array('a', 'd', 'd', 'r', 0, 0, 0, 0, 0, 0, 0, 0)
    case _ => Array(0,0,0,0,0,0,0,0,0,0,0,0)
  }

  def create(payload: Message): Header =
    Header(
      magic,
      getMessageName(payload),
      payload.toArray.length,
      sha256.digest(sha256.digest(payload.toArray)).take(4))

  def verAckFromByteArray(raw: Array[Byte]) : Either[Unit, Header] = {
    val header = fromByteArray(raw)

    (header.commandName.sameElements(getMessageName(VerAck()))) match {
      case true => Right(header)
      case false => Left()
    }
  }


  def fromByteArray(raw: Array[Byte]) : Header =
    Header(
      raw.take(4).toIntFromBigEndian,
      raw.drop(4).take(12),
      raw.drop(16).take(4).toIntFromLittleEndian,
      raw.drop(20).take(4)
    )

}