package io.bitcoinclient

import io.bitcoinclient.BitcoinClient._
import io.bitcoinclient.utils.Debug

object Main extends App {

  val app= for {
    connection <- Connection.create("198.1.231.6", 8333)
    clinet = BitcoinClient(connection)
    resp <- clinet.connect
    _ <- clinet.listen
    //_ <- clinet.mempool
    //_ <- clinet.getAddr
  } yield()

  app.unsafeRunSync()

}
