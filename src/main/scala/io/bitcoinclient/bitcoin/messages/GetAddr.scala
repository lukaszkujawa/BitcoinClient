package io.bitcoinclient.bitcoin.messages

case class GetAddr() extends Message {

  def toArray: Array[Byte] = Array()

}

object GetAddr {

  def fromByteArray(raw: Array[Byte]) = VerAck()

}
