package io.github.montokapro.set

import io.finch._
import org.scalatest.FunSpec
import io.github.montokapro.set.Cut._

class CutSpec extends FunSpec {
  describe("Cut") {
    import cats.syntax._
    import cats.syntax.semigroupk._
    import Cut._

    it("should have an empty") {
      assert(Cut.monoid.empty == Cut(None, None))
    }

    it("should compose empty") {
      val actual = Cut[Int](None, Some(Set.empty)) <+> Cut(Some(Set(1)), None)
      val expected = Cut[Int](Some(Set(1)), Some(Set.empty))
      assert(actual == expected)
    }

    it("should compose single") {
      val single = Cut(
        Some(Set(1)),
        Some(Set(2))
      )
      assert((single <+> single) == single)
    }

    it("should compose multiple") {
      val left = Cut(
        Some(Set(
          1,
          2
        )),
        Some(Set(
          4,
          5
        ))
      )
      val right = Cut(
        Some(Set(
          2,
          3
        )),
        Some(Set(
          5,
          6
        ))
      )
      val expected = Cut(
        Some(Set(
          2
        )),
        Some(Set(
          4,
          5,
          6
        ))
      )
      assert((left <+> right) == expected)
    }

    it("should respect law of the middle") {
      val actual = Cut(
        Some(Set(
          1
        )),
        Some(Set(
          1
        ))
      )
      assert(Cut.reduce(actual) == Cut.monoid.empty)
    }
  }
}
