package io.github.montokapro.set

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

case class Cut[T](intersect: Option[Set[T]], union: Option[Set[T]])

object Cut {
  import cats.Monoid
  import cats.MonoidK
  import cats.instances.option._
  import cats.syntax.apply._
  import cats.instances.invariant._

  implicit val monoid: MonoidK[Cut] = new MonoidK[Cut] {
    implicit def monoid[A]: Monoid[Cut[A]] = (
      {
        implicit val intersect = new SetInstances.SetIntersectSemilattice[A]
        Monoid[Option[Set[A]]]
      },
      {
        implicit val union = new SetInstances.SetUnionSemilattice[A]
        Monoid[Option[Set[A]]]
      }
    ).imapN(Cut.apply _)(Cut.unapply(_).get)

    // A cut diverges if it represents all values and no values simultaneously
    def empty[A]: Cut[A] = monoid[A].empty

    def combineK[A](x: Cut[A], y: Cut[A]): Cut[A] = monoid.combine(x, y)
  }

  def reduce[A](cut: Cut[A]): Cut[A] = cut match {
    case Cut(Some(intersect), Some(union)) if ((intersect intersect union).nonEmpty) => monoid.empty
    case _ => cut
  }
}
