package io.bitcoinclient.messages

import scodec.Err

case class VerAck() extends Message {

  def toEither: Either[Err, Array[Byte]] = Right(Array[Byte]())

}

object VerAck {

  val commandName = "verack"

}