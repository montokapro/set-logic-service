package io.github.montokapro.set

import io.finch._
import org.scalatest.FunSpec
import io.github.montokapro.set.Logic._

class LogicSpec extends FunSpec {
  describe("set") {
    it("should deduplicate expr") {
      val actual = Set(1, 1)
      val expected = Set(1)
      assert(actual == expected)
    }
  }

  describe("Group") {
    import cats.syntax._
    import cats.syntax.semigroup._
    // import cats.syntax.monoid._
    import Group._

    it("should have an empty") {
      assert(Group.monoid.empty == Group(None, None))
    }

    it("should compose empty") {
      val actual = Group(None, Some(Set.empty)) |+| Group(Some(Set(Id(1))), None)
      val expected = Group(Some(Set(Id(1))), Some(Set.empty))
      assert(actual == expected)
    }

    it("should compose single") {
      val single = Group(
        Some(Set(Id(1))),
        Some(Set(Id(1)))
      )
      val actual = single |+| single
      assert((single |+| single) == single)
    }

    it("should compose multiple") {
      val left = Group(
        Some(Set(
          Id(1),
          Id(2)
        )),
        Some(Set(
          Id(1),
          Id(2)
        ))
      )
      val right = Group(
        Some(Set(
          Id(2),
          Id(3)
        )),
        Some(Set(
          Id(2),
          Id(3)
        ))
      )
      val expected = Group(
        Some(Set(
          Id(2)
        )),
        Some(Set(
          Id(1),
          Id(2),
          Id(3)
        ))
      )
      assert((left |+| right) == expected)
    }
  }
}
