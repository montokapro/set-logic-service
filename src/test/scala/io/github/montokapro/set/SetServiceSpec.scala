package io.github.montokapro.set

import io.finch._
import org.scalatest.FunSpec
import io.github.montokapro.set.SetService._

class SetServiceSpec extends FunSpec {
  describe("set") {
    it("should deduplicate expr") {
      val actual = Set(Lit("a"), Lit("a"))
      val expected = Set(Lit("a"))
      assert(actual == expected)
    }
  }

  describe("singleOption") {
    it("should process empty") {
      val actual = Set.empty
      val expected = None
      assert(singleOption(actual) == expected)
    }

    it("should process single") {
      val actual = Set(1)
      val expected = Some(1)
      assert(singleOption(actual) == expected)
    }

    it("should process multiple") {
      val actual = Set(1, 2)
      val expected = None
      assert(singleOption(actual) == expected)
    }

    it("should process even more") {
      val actual = Set(1, 2, 3)
      val expected = None
      assert(singleOption(actual) == expected)
    }
  }

  describe("And") {
    it("should simplify single") {
      val actual = And(Set(
        Lit("a"),
      ))
      val expected = Lit("a")
      assert(Expr.reduce(actual) == expected)
    }

    it("should reduce nested") {
      val actual = And(Set(
        And(Set(
          Lit("a"),
          Lit("b")
        )),
        And(Set(
          Lit("b"),
          Lit("c")
        ))
      ))
      val expected = And(Set.empty)
      assert(Expr.reduce(actual) == expected)
    }

    it("should reduce empty") {
      val actual = And(Set(
        And(Set(
          Lit("a"),
          Lit("b")
        )),
        And(Set.empty)
      ))
      val expected = And(Set.empty)
      assert(Expr.reduce(actual) == expected)
    }
  }

  describe("Or") {
    it("should simplify single") {
      val actual = Or(Set(
        Lit("a")
      ))
      val expected = Lit("a")
      assert(Expr.reduce(actual) == expected)
    }

    it("should reduce nested") {
      val actual = Or(Set(
        Or(Set(
          Lit("a"),
          Lit("b")
        )),
        Or(Set(
          Lit("b"),
          Lit("c")
        ))
      ))
      val expected = Or(Set(
        Lit("a"),
        Lit("b"),
        Lit("c")
      ))
      assert(Expr.reduce(actual) == expected)
    }

    it("should reduce empty") {
      val actual = Or(Set(
        Or(Set(
          Lit("a"),
          Lit("b")
        )),
        Or(Set.empty)
      ))
      val expected = Or(Set(
        Lit("a"),
        Lit("b")
      ))
      assert(Expr.reduce(actual) == expected)
    }
  }
}
