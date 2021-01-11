package io.bitcoinclient.bitcoin.messages

case class Addr() extends Message {

  def toArray: Array[Byte] = Array()

}


object Addr {

  def fromByteArray(raw: Array[Byte]) =  Addr()

}
