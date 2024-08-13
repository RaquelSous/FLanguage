package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._


class ParserTest extends AnyFlatSpec with should.Matchers {

  "An integer expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("42")
    result.get should be(CInt(42))

    val result2 = FLangParser.parseCode("-15")
    result2.get should be(CInt(-15))
  }

  "An identifier expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("x")
    result.get should be(Id("x"))

    val result2 = FLangParser.parseCode("_var123")
    result2.get should be(Id("_var123"))
  }

  "An addition expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("add 1 2")
    result.get should be(Add(CInt(1), CInt(2)))
  }

  "A multiplication expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("mul 3 4")
    result.get should be(Mul(CInt(3), CInt(4)))
  }

  "A conditional expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("if 1 then 2 else 3")
    result.get should be(IfThenElse(CInt(1), CInt(2), CInt(3)))

    val result2 = FLangParser.parseCode("if x then y else z")
    result2.get should be(IfThenElse(Id("x"), Id("y"), Id("z")))
  }

  "A function application expression" should "be parsed correctly" in {
    val result = FLangParser.parseCode("app inc 5")
    result.get should be(App("inc", CInt(5)))
  }

  "An expression with nested addition" should "be parsed correctly" in {
    val code = "add 1 add 2 3"
    val result = FLangParser.parseCode(code)
    result.get should be(Add(CInt(1), Add(CInt(2), CInt(3))))
  }

  "An expression with nested multiplication" should "be parsed correctly" in {
    val code = "mul 2 mul 3 4"
    val result = FLangParser.parseCode(code)
    result.get should be(Mul(CInt(2), Mul(CInt(3), CInt(4))))
  }

  "A complex expression with function application" should "be parsed correctly" in {
    val code = "app f add 1 2"
    val result = FLangParser.parseCode(code)
    result.get should be(App("f", Add(CInt(1), CInt(2))))
  }

  "A complex expression with condition and nested operations" should "be parsed correctly" in {
    val code = "if add 1 mul 2 3 then mul 4 5 else app g add 6 7"
    val result = FLangParser.parseCode(code)
    result.get should be(
      IfThenElse(
        Add(CInt(1), Mul(CInt(2), CInt(3))),
        Mul(CInt(4), CInt(5)),
        App("g", Add(CInt(6), CInt(7)))
      )
    )
  }
}
