package io.bitcoinclient.utils

object Debug {
  def dumpBinary(stream: Array[Byte]): Unit = println(stream.foldLeft("" : String)(
    (acc, b) => (acc.length > 0 && acc.length % 48 == 45) match {
      case true => acc + ("%02X\n" format b)
      case _ => acc + ("%02X " format b)
    }
  ))
}
