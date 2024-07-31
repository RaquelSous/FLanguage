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

//  "An IfThenElse expression" should "evaluate to the correct branch based on the condition" in{
//    val expr = IfThenElse(CTrue(), CInt(1), CInt(0))
//    val result = Interpreter.eval(expr, List()).f(List())._1
//    result should be(1)
//
//        val expr2 = IfThenElse(CFalse(), CInt(1), CInt(0))
//    val result2 = Interpreter.eval(expr2, List()).f(List())._1
//    result2 should be(0)
//
//  }
//
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
