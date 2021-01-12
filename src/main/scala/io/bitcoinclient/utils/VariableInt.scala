package io.bitcoinclient.utils

import Endianess._

object VariableInt {

  def parse(buff: Array[Byte]): Int = (buff.head & 0xFF) match {
    case 0xFF => 0
    case 0xFE => buff.tail.take(4).toIntFromLittleEndian
    case 0xFD => buff.tail.take(2).toShortFromLittleEndian & 0xFFFF
    case _ => buff.head & 0xFF
  }

}
