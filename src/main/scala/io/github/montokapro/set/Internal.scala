package io.github.montokapro.set

/*
 * This exercise has seemed to reveal the need for a
 * partial order on expressions and cuts.
 *
 * Cutting needs to be able to prune using this partial order
 */
object Internal {
  sealed trait Expr
  case class And(cut: Cut[Either[Int, Expr]]) extends Expr
  case class Or(cut: Cut[Either[Int, Expr]]) extends Expr

  object Expr {
    def reduce(expr: Expr): Expr = expr
  }

  object And {
    def reduce(and: And): Expr = and
  }

  object Or {
    def reduce(or: Or): Expr = or
  }
}
