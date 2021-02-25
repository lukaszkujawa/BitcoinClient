package io.bitcoinclient


import io.bitcoinclient.bitcoin.{Client, Connection}

object BTCP2P extends App {


  import cats.effect._
  import cats.implicits._


  val app= for {
    connection <- Connection.create("71.231.145.228", 8333)
    client = Client(connection)
    resp <- client.connect

    //_ = dumpBinary(addr)
    _ <- client.listen
    _ <- client.getAddresses
    //_ <- connection.close
  } yield (resp)

  app.unsafeRunSync()

}
