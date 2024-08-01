package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import StateMonad._

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  val inc = FDeclaration("inc", "x", Add(Id("x"), CInt(1)))

  val declarations = List(inc)

  val initialState: S = List()

  "An IfThenElse expression" should "evaluate to the correct branch based on the condition" in{
    val expr = IfThenElse(CTrue(), CInt(1), CInt(0))
    val result = Interpreter.eval(expr, List()).f(List())._1
    result should be(1)

        val expr2 = IfThenElse(CFalse(), CInt(1), CInt(0))
    val result2 = Interpreter.eval(expr2, List()).f(List())._1
    result2 should be(0)

  }

  "An IfThenElse expression" should "evaluate to the true branch if the condition is true in" in{

    val expr = IfThenElse(CTrue(), CInt(1), CInt(0))
    val result = Interpreter.eval(expr, List()).f(List())._1
    result should be(1)
  }
  
  "An IfThenElse expression" should "evaluate to the false branch if the condition is false in" in{

    val expr2 = IfThenElse(CFalse(), CInt(1), CInt(0))
    val result2 = Interpreter.eval(expr2, List()).f(List())._1
    result2 should be(0)
  }

  "eval Ctrue()" should "return an boolean 1" in {
    val cTrue = CTrue()
    val (res, _) = runState(eval(cTrue, declarations))(initialState)
    res should be(1)
  }

  "eval CFalse()" should "return an boolean 0" in {
    val cFalse = CFalse()
    val (res, _) = runState(eval(cFalse, declarations))(initialState)
    res should be(0)
  }

  "eval Not(CTrue())" should "return an boolean 0" in {
    val notTrue = Not(CTrue())
    val (res, _) = runState(eval(notTrue, declarations))(initialState)
    res should be(0)
  }

  "eval Not(CFalse())" should "return an boolean 1" in {
    val notFalse = Not(CFalse())
    val (res, _) = runState(eval(notFalse, declarations))(initialState)
    res should be(1)
  }
  "Eval And(CTrue(), CTrue())" should "return an boolean 1" in {
    val AndTrue = And(CTrue(), CTrue())
    val (res, _) = runState(eval(AndTrue, declarations))(initialState)
    res should be(1)
  }
  "Eval And(CTrue(), CFalse())" should "return an boolean 0" in {
    val AndFalse = And(CTrue(), CFalse())
    val (res, _) = runState(eval(AndFalse, declarations))(initialState)
    res should be(0)
  }
  "Eval And(CFalse(), CFalse())" should "return an boolean 0" in {
    val AndFalse = And(CFalse(), CFalse())
    val (res, _) = runState(eval(AndFalse, declarations))(initialState)
    res should be(0)
  }
  "Eval Or(CTrue(), CFalse())" should "return an boolean 1" in {
    val OrTrue = Or(CTrue(), CFalse())
    val (res, _) = runState(eval(OrTrue, declarations))(initialState)
    res should be(1)
  }
  "Eval Or(CFalse(), CFalse())" should "return an boolean 0" in {
    val OrFalse = Or(CFalse(), CFalse())
    val (res, _) = runState(eval(OrFalse, declarations))(initialState)
    res should be(0)
  }
  "Eval Or(CTrue(), CTrue())" should "return an boolean 1" in {
    val OrFalse = Or(CTrue(), CTrue())
    val (res, _) = runState(eval(OrFalse, declarations))(initialState)
    res should be(1)
  }

  "Eval CEquals(CInt(1), CInt(2))" should "return an boolean 0" in {
    val expr = CEquals(CInt(1), CInt(2))
    val (res, _) = runState(eval(expr, declarations))(initialState)
    res should be(0)
  }

  "Eval CEquals(CInt(2), CInt(2))" should "return an boolean 1" in {
    val expr = CEquals(CInt(2), CInt(2))
    val (res, _) = runState(eval(expr, declarations))(initialState)
    res should be(1)
  }

  "Eval Or(CEquals(CInt(1), CInt(1)), CEquals(CInt(1), CInt(2)))" should "return an boolean 1" in {
    val expr = Or(CEquals(CInt(1), CInt(1)), CEquals(CInt(1), CInt(2)))
    val (res, _) = runState(eval(expr, declarations))(initialState)
    res should be(1)
  }
  "Eval Or(CEquals(CInt(1), CInt(1)), CEquals(CInt(1), CInt(1)))" should "return an boolean 1" in {
    val expr = Or(CEquals(CInt(1), CInt(1)), CEquals(CInt(1), CInt(1)))
    val (res, _) = runState(eval(expr, declarations))(initialState)
    res should be(1)
  }
//  "eval CInt(5)" should "return an integer value 5." in {
//    val c5 = CInt(5)
//    val (res, _) = runState(eval(c5, declarations))(initialState)
//    res should be (5)
//  }
//
//  "eval Add(CInt(5), CInt(10)) " should "return an integer value 15." in {
//    val c5  = CInt(5)
//    val c10 = CInt(10)
//    val add = Add(c5, c10)
//    val (res, _) = runState(eval(add, declarations))(initialState)
//    res should be (15)
//  }
//
//  "eval Add(CInt(5), Add(CInt(5), CInt(10))) " should "return an integer value 20." in {
//    val c5 = CInt(5)
//    val c10 = CInt(10)
//    val add = Add(c5, Add(c5, c10))
//    val (res, _) = runState(eval(add, declarations))(initialState)
//    res should be(20)
//  }
//
//  "eval Mul(CInt(5), CInt(10))" should "return an integer value 50" in {
//    val c5 = CInt(5)
//    val c10 = CInt(10)
//    val mul = Mul(c5, CInt(10))
//    val (res, _) = runState(eval(mul, declarations))(initialState)
//    res should be(50)
//  }
//
//  "eval App(inc, 99) " should "return an integer value 100" in {
//    val app = App("inc", CInt(99))
//    val (res, _) = runState(eval(app, declarations))(initialState)
//    res should be (100)
//  }
}
