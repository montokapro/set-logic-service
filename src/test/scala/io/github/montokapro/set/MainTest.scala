package io.github.montokapro.set

import io.finch._
import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("healthcheck") {
    assert(Main.healthcheck(Input.get("/")).awaitValueUnsafe() == Some("OK"))
  }
}
