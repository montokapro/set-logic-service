package io.github.montokapro.set

object SetService {
  sealed trait Expr
  case class And(and: Set[Expr]) extends Expr
  case class Or(or: Set[Expr]) extends Expr
  case object Top extends Expr // Everything matches
  case object Bottom extends Expr // Nothing matches
  case class Not(not: Expr) extends Expr
  case class Id(id: Int) extends Expr

  case class PropA(a: String) extends Expr
  case class PropB(b: String) extends Expr


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
      case Not(expr) => Not.reduce(expr)
      case And(set) => And.reduce(set)
      case Or(set) => Or.reduce(set)
      case _ => expr
    }
  }

  object Not {
    def reduce(expr: Expr): Expr = Not(Expr.reduce(expr))
  }

  object Or {
    import cats._
    import cats.data._
    import cats.syntax._
    import cats.instances.set._
    import cats.kernel.CommutativeMonoid

    def combine(x: Expr, y: Expr): Expr = (x, y) match {
      case (Top, _) => Top
      case (_, Top) => Top
      case (Or(a), Or(b)) => Or(a union b)
      case (Or(a), Id(b)) => Or(a + Id(b))
      case (Id(a), Or(b)) => Or(b + Id(a))
      case (Not(Id(_)), Not(Id(_))) => Top
      case (a: Expr, b: Expr) => Or(Set(a, b))
    }

    def reduce(or: Set[Expr]): Expr = {
      if (or.isEmpty) {
        Top
      } else {
        val expr = or
          .map(Expr.reduce)
          .reduce(combine)
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

    def combine(x: Expr, y: Expr): Expr = (x, y) match {
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (And(a), And(b)) => And(a intersect b)
      case (And(set), id@Id(_)) => if (set.exists(_ != id)) Bottom else id
      case (id@Id(_), And(set)) => if (set.exists(_ != id)) Bottom else id
      case (a@Id(_), b@Id(_)) => Bottom
      case (a: Expr, b: Expr) => And(Set(a, b))
    }

    def reduce(and: Set[Expr]): Expr = {
      if (and.isEmpty) {
        Bottom
      } else {
        val expr = and
          .map(Expr.reduce)
          .reduce(combine)
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
      case id @ Id(_) => id.asJson
    }

    implicit val decodeExpr: Decoder[Expr] =
      List[Decoder[Expr]](
        Decoder[And].widen,
        Decoder[Or].widen,
        Decoder[Id].widen
      ).reduceLeft(_ or _)
  }
}
