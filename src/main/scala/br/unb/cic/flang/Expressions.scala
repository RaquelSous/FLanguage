package br.unb.cic.flang

sealed trait Expr

case class CInt(v: Integer) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(name: String, arg: Expr) extends Expr

// booleans
case class CTrue() extends Expr
case class CFalse() extends Expr

// boolean operators
case class Not(e: Expr) extends Expr
case class And(lhs: Expr, rhs: Expr) extends Expr
case class Or(lhs: Expr, rhs: Expr) extends Expr
case class CEquals(lhs: Expr, rhs: Expr) extends Expr // comparações de igualdade

case class IfThenElse(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
