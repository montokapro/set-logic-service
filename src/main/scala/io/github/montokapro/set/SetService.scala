package io.github.montokapro.set

object SetService {
  sealed trait Expr
  case class And(and: Set[Expr]) extends Expr
  case class Or(or: Set[Expr]) extends Expr
  case class Lit(lit: String) extends Expr
  object Empty extends Expr

  // Grab the item in the set if it's the only item
  // Avoids checking the set size, which is theoretically costly to compute
  // This is probably slower in practice!
  def singleOption[A](set: Set[A]): Option[A] = {
    import cats._
    import cats.data._
    import cats.instances.set._
    import cats.kernel.CommutativeMonoid

    val monoid: CommutativeMonoid[Option[A]] = new CommutativeMonoid[Option[A]] {
      def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
        case (some@Some(_), None) => some
        case _ => None
      }
      def empty: Option[A] = None
    }

    def toOption(a: A): Option[A] = Some(a)

    UnorderedFoldable[Set].unorderedFoldMap(set)(toOption)(monoid)
  }

  object Expr {
    def reduce(expr: Expr): Expr = expr match {
      case And(and) => And.reduce(and)
      case Or(or) => Or.reduce(or)
      case lit@Lit(_) => lit
    }
  }

  object Or {
    import cats._
    import cats.data._
    import cats.syntax._
    import cats.instances.set._
    import cats.kernel.CommutativeMonoid

    implicit val monoid = new CommutativeMonoid[Expr] {
      def combine(x: Expr, y: Expr): Expr = (x, y) match {
        case (Or(a), Or(b)) => Or(a union b)
        case (Or(a), Lit(b)) => Or(a + Lit(b))
        case (Lit(a), Or(b)) => Or(b + Lit(a))
        case (a: Expr, b: Expr) => Or(Set(a, b))
      }

      def empty: Expr = Or(Set.empty) // Top type, matches everything
    }

    def reduce(or: Set[Expr]): Expr = {
      if (or.isEmpty) {
        Or(or)
      } else {
        val expr = UnorderedFoldable[Set].unorderedFoldMap(or)(Expr.reduce)
        expr match {
          case Or(set) if set.size == 1 => set.head // TODO: don't read full size, only 2 elements
          case _ => expr
        }
      }
    }
  }

  object And {
    import cats._
    import cats.data._
    import cats.syntax._
    import cats.instances.set._
    import cats.kernel.CommutativeMonoid

    implicit val monoid = new CommutativeMonoid[Expr] {
      def combine(x: Expr, y: Expr): Expr = (x, y) match {
        case (And(a), And(b)) => And(a intersect b)
        case (And(a), Lit(b)) => And(a intersect Set(Lit(b)))
        case (Lit(a), And(b)) => And(b intersect Set(Lit(a)))
        case (a: Expr, b: Expr) => And(Set(a) intersect Set(b))
      }

      def empty: Expr = And(Set.empty) // Bottom type, matches nothing
    }

    def reduce(and: Set[Expr]): Expr = {
      if (and.isEmpty) {
        And(and)
      } else {
        val expr = and.map(Expr.reduce).reduce(monoid.combine)
        // val expr = UnorderedFoldable[Set].unorderedFoldMap(and)(Expr.reduce)
        expr match {
          case And(set) if set.size == 1 => set.head // TODO: don't read full size, only 2 elements
          case _ => expr
        }
      }
    }
  }

  object GenericDerivation {
    import cats.syntax.functor._
    import io.circe.{ Decoder, Encoder }, io.circe.generic.auto._
    import io.circe.syntax._

    implicit val encodeExpr: Encoder[Expr] = Encoder.instance {
      case and @ And(_) => and.asJson
      case or @ Or(_) => or.asJson
      case lit @ Lit(_) => lit.asJson
    }

    implicit val decodeExpr: Decoder[Expr] =
      List[Decoder[Expr]](
        Decoder[And].widen,
        Decoder[Or].widen,
        Decoder[Lit].widen
      ).reduceLeft(_ or _)
  }
}
