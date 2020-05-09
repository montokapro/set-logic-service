package io.github.montokapro.set

import cats.effect.IO
import cats.effect.concurrent.Ref
import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.Await
import io.finch._
import io.finch.catsEffect._
import io.finch.circe._
import io.circe.generic.auto._
import com.twitter.util.Memoize

object Main extends App {
  case class Message(hello: String)

  def healthcheck: Endpoint[IO, String] = get(pathEmpty) {
    Ok("OK")
  }

  val server = {
    val service = Bootstrap
      .serve[Text.Plain](healthcheck)
      .toService

    Http.server.serve(":8081", service)
  }

  Await.ready(server)
}
