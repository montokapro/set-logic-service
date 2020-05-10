package io.github.montokapro.set

import cats.data.NonEmptySet
import java.util.concurrent.Semaphore

object Logic {
  sealed trait Op
  case object And extends Op
  case object Or extends Op

  sealed trait Expr
  case class Id(id: Int) extends Expr
  case class Not(not: Expr) extends Expr
  case class Group(intersect: Option[Set[Expr]], union: Option[Set[Expr]]) extends Expr

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

    val monoidWithoutReduction: Monoid[Group] = (
      {
        implicit val intersect = new SetInstances.SetIntersectSemilattice[Expr]
        Monoid[Option[Set[Expr]]]
      },
      {
        implicit val union = new SetInstances.SetUnionSemilattice[Expr]
        Monoid[Option[Set[Expr]]]
      }
    ).imapN(Group.apply _)(Group.unapply(_).get)

    // TODO: Reduce the number of redundant intersections
    // during the reduction law of the excluded middle
    //
    // Consider making a custom monoid without using
    // monoidWithoutReduction
    implicit val monoid: Monoid[Group] = new Monoid[Group] {
      def combine(a: Group, b: Group): Group = Group.reduce({
        monoidWithoutReduction.combine(a, b)
      })

      def empty: Group = monoidWithoutReduction.empty
    }

    // A group cannot represent both all values and no values simultaneously
    val invalid = Group(
      Some(Set.empty), // Matches no values
      Some(Set.empty) // Matches all values
    )

    def reduce(group: Group): Group = group match {
      case Group(Some(intersect), Some(union)) if ((intersect intersect union).nonEmpty) => invalid
      case _ => group
    }
  }
}
