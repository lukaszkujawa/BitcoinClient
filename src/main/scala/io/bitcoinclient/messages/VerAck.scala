package io.bitcoinclient.messages

import scodec.Codec

case class VerAck() {

}

object VerAck {

  val commandName = "verack"

}