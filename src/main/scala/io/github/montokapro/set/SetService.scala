package io.github.montokapro.set

object SetService {
  sealed trait Expr
  case class And(and: Set[Expr]) extends Expr
  case class Or(or: Set[Expr]) extends Expr
  case class Lit(lit: String) extends Expr
  object Empty extends Expr

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
      def combine(x: Expr, y: Expr): Expr = {
        (Expr.reduce(x), Expr.reduce(y)) match {
          case (Or(a), Or(b)) => Or(a ++ b)
          case (Or(a), Lit(b)) => Or(a + Lit(b))
          case (Lit(a), Or(b)) => Or(b + Lit(a))
          case (Lit(a), Lit(b)) => Or(Set(Lit(b), Lit(a)))
          case (a: Expr, b: Expr) => Or(Set(a, b)) // TODO
        }
      }

      def empty: Expr = Or(Set.empty) // All
    }

    def reduce(or: Set[Expr]): Expr =
      UnorderedFoldable[Set].unorderedFold(or)
  }

  object And {
    import cats._
    import cats.data._
    import cats.syntax._
    import cats.instances.set._
    import cats.kernel.CommutativeMonoid

    implicit val monoid = new CommutativeMonoid[Expr] {
      def combine(x: Expr, y: Expr): Expr = {
        (Expr.reduce(x), Expr.reduce(y)) match {
          case (And(a), And(b)) => And(a ++ b)
          case (And(a), Lit(b)) => And(a + Lit(b))
          case (Lit(a), And(b)) => And(b + Lit(a))
          case (Lit(a), Lit(b)) => And(Set(Lit(b), Lit(a)))
          case (a: Expr, b: Expr) => And(Set(a, b)) // TODO
        }
      }

      def empty: Expr = And(Set.empty) // None
    }

    def reduce(and: Set[Expr]): Expr =
      UnorderedFoldable[Set].unorderedFold(and)
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
