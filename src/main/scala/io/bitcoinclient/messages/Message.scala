package io.bitcoinclient.messages

import scodec.Err

trait Message {

  def toEither: Either[Err, Array[Byte]]

}
