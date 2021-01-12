package io.bitcoinclient.bitcoin.messages

case class VerAck() extends Message {

  def toArray: Array[Byte] = Array()

}

object VerAck {

  def fromByteArray(raw: Array[Byte]) = VerAck()

}
