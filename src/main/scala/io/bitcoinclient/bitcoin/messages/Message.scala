package io.bitcoinclient.bitcoin.messages

trait Message {
  def toArray: Array[Byte]
}
