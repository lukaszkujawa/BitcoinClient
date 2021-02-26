package io.bitcoinclient.messages

import scodec.Err

case class GetAddr() extends Message {

  def toEither: Either[Err, Array[Byte]] = Right(Array[Byte]())

}

object GetAddr {

  val commandName = "getaddr"

}