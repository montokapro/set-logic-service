package io.github.montokapro.set

import org.scalatest.FunSpec
import io.github.montokapro.set.Internal._

class InternalSpec extends FunSpec {
  describe("And") {
    it("should reduced positive nested and") {
      val actual = And(Cut(
        // pos
        Some(Set(
          Left(1),
          Right(And(Cut(
            Some(Set(
              Left(2)
            )),
            Some(Set(
              Left(3)
            ))
          )))
        )),
        // neg
        None
      ))
      val expected = And(Cut(
        Some(Set.empty),
        Some(Set(
          Left(3)
        ))
      ))
      assert(And.reduce(actual) == expected)
    }

    it("should reduced negative nested and") {
      val actual = And(Cut(
        // pos
        None,
        // neg
        Some(Set(
          Left(1),
          Right(And(Cut(
            Some(Set(
              Left(2)
            )),
            Some(Set(
              Left(3)
            ))
          )))
        ))
      ))
      val expected = And(Cut(
        // pos
        Some(Set(
          Right(Or(Cut(
            Some(Set(
              Left(3)
            )),
            Some(Set(
              Left(2)
            ))
          )))
        )),
        // neg
        Some(Set(
          Left(1)
        ))
      ))
      assert(And.reduce(actual) == expected)
    }
  }

  describe("Or") {
    it("should reduced positive nested or") {
      val actual = Or(Cut(
        // neg
        None,
        // pos
        Some(Set(
          Left(1),
          Right(Or(Cut(
            Some(Set(
              Left(2)
            )),
            Some(Set(
              Left(3)
            ))
          )))
        ))
      ))
      val expected = Or(Cut(
        // neg
        Some(Set(
          Left(2)
        )),
        // pos
        Some(Set(
          Left(1),
          Left(3)
        ))
      ))
      assert(Or.reduce(actual) == expected)
    }

    it("should reduced negative nested or") {
      val actual = Or(Cut(
        // neg
        Some(Set(
          Left(1),
          Right(Or(Cut(
            Some(Set(
              Left(2)
            )),
            Some(Set(
              Left(3)
            ))
          )))
        )),
        // pos
        None
      ))
      val expected = Or(Cut(
        // neg
        Some(Set(
          Left(1)
        )),
        // pos
        Some(Set(
          Right(And(Cut(
            Some(Set(
              Left(3)
            )),
            Some(Set(
              Left(2)
            ))
          )))
        ))
      ))
      assert(Or.reduce(actual) == expected)
    }
  }
}
