package br.unb.cic.flang

import scala.util.parsing.combinator._

object FLangParser extends RegexParsers {
  sealed trait Expr
  case class CInt(v: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Mul(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
  case class Id(name: String) extends Expr
  case class App(func: Expr, arg: Expr) extends Expr
  case class Lambda(param: String, body: Expr) extends Expr
  case class IfThenElse(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
  case class WhileLoop(cond: Expr, body: Expr) extends Expr
  case class CTrue() extends Expr
  case class CFalse() extends Expr
  case class Not(e: Expr) extends Expr
  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr) extends Expr
  case class Equals(lhs: Expr, rhs: Expr) extends Expr

  def variable: Parser[String] = """[a-zA-Z_][a-zA-Z_0-9]*""".r
  def number: Parser[Expr] = """\d+""".r ^^ { n => CInt(n.toInt) }
  def varParser: Parser[Expr] = variable ^^ { v => Id(v) }
  def lambda: Parser[Expr] = "(" ~> "lambda" ~> variable ~ expr <~ ")" ^^ {
    case param ~ body => Lambda(param, body)
  }
  def app: Parser[Expr] = "(" ~> expr ~ expr <~ ")" ^^ {
    case func ~ arg => App(func, arg)
  }
  def defParser: Parser[Expr] = "(" ~> "def" ~> variable ~ ":" ~ expr ~ "=" ~ expr <~ ")" ^^ {
    case name ~ ":" ~ typ ~ "=" ~ value => App(Id(name), value)
  }
  def ifThenElse: Parser[Expr] = "(" ~> "if" ~> expr ~ expr ~ expr <~ ")" ^^ {
    case cond ~ ifTrue ~ ifFalse => IfThenElse(cond, ifTrue, ifFalse)
  }
  def whileLoop: Parser[Expr] = "(" ~> "while" ~> expr ~ expr <~ ")" ^^ {
    case cond ~ body => WhileLoop(cond, body)
  }
  def notExpr: Parser[Expr] = "(" ~> "not" ~> expr <~ ")" ^^ { e => Not(e) }
  def andExpr: Parser[Expr] = "(" ~> "and" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => And(lhs, rhs)
  }
  def orExpr: Parser[Expr] = "(" ~> "or" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Or(lhs, rhs)
  }
  def equalsExpr: Parser[Expr] = "(" ~> "equals" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Equals(lhs, rhs)
  }
  def arithExpr: Parser[Expr] = addExpr | subExpr | mulExpr | divExpr | number
  def addExpr: Parser[Expr] = "(" ~> "+" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Add(lhs, rhs)
  }
  def subExpr: Parser[Expr] = "(" ~> "-" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Sub(lhs, rhs)
  }
  def mulExpr: Parser[Expr] = "(" ~> "*" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Mul(lhs, rhs)
  }
  def divExpr: Parser[Expr] = "(" ~> "/" ~> expr ~ expr <~ ")" ^^ {
    case lhs ~ rhs => Div(lhs, rhs)
  }
  def logicExpr: Parser[Expr] = notExpr | andExpr | orExpr | equalsExpr
  def expr: Parser[Expr] = defParser | app | ifThenElse | whileLoop | lambda | logicExpr | arithExpr | varParser
  def parse(input: String): ParseResult[Expr] = parseAll(expr, input)
}
