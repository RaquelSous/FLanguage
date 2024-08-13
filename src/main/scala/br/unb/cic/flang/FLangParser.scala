package br.unb.cic.flang

import scala.util.parsing.combinator._

object FLangParser extends RegexParsers {

  def intExpr: Parser[CInt] = "-?[0-9]+".r ^^ { i => CInt(i.toInt) }

  def idExpr: Parser[Id] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { Id(_) }

  def addExpr: Parser[Add] =
    "add" ~> expr ~ expr ^^ {
      case lhs ~ rhs => Add(lhs, rhs)
    }

  def mulExpr: Parser[Mul] =
    "mul" ~> expr ~ expr ^^ {
      case lhs ~ rhs => Mul(lhs, rhs)
    }

  def ifExpr: Parser[IfThenElse] =
    "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr ^^ {
      case cond ~ "then" ~ ifTrue ~ "else" ~ ifFalse => IfThenElse(cond, ifTrue, ifFalse)
    }

  def appExpr: Parser[App] =
    "app" ~> idExpr ~ expr ^^ {
      case Id(name) ~ arg => App(name, arg)
    }

  def expr: Parser[Expr] = ifExpr | appExpr | addExpr | mulExpr | intExpr | idExpr

  def parseCode(code: String): ParseResult[Expr] = parseAll(expr, code)
}
