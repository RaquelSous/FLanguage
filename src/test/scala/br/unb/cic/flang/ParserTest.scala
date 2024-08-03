package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._
import FLangParser._

class ParserTest extends AnyFlatSpec with should.Matchers {

  "A CInt expression" should "be parsed correctly" in {
    val input = "123"
    parse(input) should be(Right(CInt(123)))
  }

  "An Add expression" should "be parsed correctly" in {
    val input = "(+ 1 2)"
    parse(input) should be(Right(Add(CInt(1), CInt(2))))
  }

  "A Sub expression" should "be parsed correctly" in {
    val input = "(- 5 3)"
    parse(input) should be(Right(Sub(CInt(5), CInt(3))))
  }

  "A Mul expression" should "be parsed correctly" in {
    val input = "(* 2 3)"
    parse(input) should be(Right(Mul(CInt(2), CInt(3))))
  }

  "A Div expression" should "be parsed correctly" in {
    val input = "(/ 6 2)"
    parse(input) should be(Right(Div(CInt(6), CInt(2))))
  }

  "An Id expression" should "be parsed correctly" in {
    val input = "x"
    parse(input) should be(Right(Id("x")))
  }

  "An App expression" should "be parsed correctly" in {
    val input = "(* 2 x)"
    parse(input) should be(Right(App(CInt(2), Id("x"))))
  }

  "A Lambda expression" should "be parsed correctly" in {
    val input = "(lambda x (+ x 1))"
    parse(input) should be(Right(Lambda("x", Add(Id("x"), CInt(1)))))
  }

  "An IfThenElse expression" should "be parsed correctly" in {
    val input = "(if true 1 0)"
    parse(input) should be(Right(IfThenElse(CTrue(), CInt(1), CInt(0))))
  }

  "A WhileLoop expression" should "be parsed correctly" in {
    val input = "(while true (+ x 1))"
    parse(input) should be(Right(WhileLoop(CTrue(), Add(Id("x"), CInt(1)))))
  }

  "A CTrue expression" should "be parsed correctly" in {
    val input = "true"
    parse(input) should be(Right(CTrue()))
  }

  "A CFalse expression" should "be parsed correctly" in {
    val input = "false"
    parse(input) should be(Right(CFalse()))
  }

  "A Not expression" should "be parsed correctly" in {
    val input = "(not true)"
    parse(input) should be(Right(Not(CTrue())))
  }

  "An And expression" should "be parsed correctly" in {
    val input = "(and true false)"
    parse(input) should be(Right(And(CTrue(), CFalse())))
  }

  "An Or expression" should "be parsed correctly" in {
    val input = "(or true false)"
    parse(input) should be(Right(Or(CTrue(), CFalse())))
  }

  "An Equals expression" should "be parsed correctly" in {
    val input = "(equals 1 2)"
    parse(input) should be(Right(Equals(CInt(1), CInt(2))))
  }

  "A complex expression" should "be parsed correctly" in {
    val input = "(if (and true false) (lambda x (+ x 1)) (not true))"
    parse(input) should be(Right(IfThenElse(And(CTrue(), CFalse()), Lambda("x", Add(Id("x"), CInt(1))), Not(CTrue()))))
  }

  "An incorrect expression" should "fail to parse" in {

  }
}