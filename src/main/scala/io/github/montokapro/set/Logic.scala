package io.github.montokapro.set

import cats.data.NonEmptySet
import java.util.concurrent.Semaphore

object Logic {
  sealed trait Op
  case object And extends Op
  case object Or extends Op

  sealed trait Expr
  case class Id(id: Int) extends Expr
  case class Group(and: Option[Set[Expr]], or: Option[Set[Expr]]) extends Expr

  object SetInstances {
    import cats.kernel.BoundedSemilattice

    class SetIntersectSemilattice[A] extends BoundedSemilattice[Set[A]] {
      def empty: Set[A] = Set.empty
      def combine(x: Set[A], y: Set[A]): Set[A] = x & y
    }

    class SetUnionSemilattice[A] extends BoundedSemilattice[Set[A]] {
      def empty: Set[A] = Set.empty
      def combine(x: Set[A], y: Set[A]): Set[A] = x | y
    }
  }

  object Group {
    import cats.Monoid
    import cats.instances.option._
    import cats.syntax.apply._
    import cats.instances.invariant._

    implicit val monoid: Monoid[Group] = (
      {
        implicit val and = new SetInstances.SetIntersectSemilattice[Expr]
        Monoid[Option[Set[Expr]]]
      },
      {
        implicit val or = new SetInstances.SetUnionSemilattice[Expr]
        Monoid[Option[Set[Expr]]]
      }
    ).imapN(Group.apply _)(Group.unapply(_).get)

    // A group cannot represent both all values and no values simultaneous
    val invalid = Group(
      Some(Set.empty), // Matches no values
      Some(Set.empty) // Matches all values
    )
  }
}
