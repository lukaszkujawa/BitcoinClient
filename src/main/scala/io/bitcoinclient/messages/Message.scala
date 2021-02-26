package io.bitcoinclient.messages

import scodec.{Codec, Err}
import io.bitcoinclient.utils.Debug
import scodec.bits.BitVector
trait Message {

  def toEither: Either[Err, Array[Byte]]

}

object Message {

  def parseMessage(header: Header, payload: Array[Byte]): Unit = {
    println("Parsing: " + header.commandName )

    if(header.commandName.contains(  Inv.commandName) ){
      println("==")
      Codec[Inv].decode(BitVector(payload)).map(println)

    }

    //Debug.dumpBinary(payload)
  }
}