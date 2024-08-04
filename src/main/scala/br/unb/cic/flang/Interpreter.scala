package br.unb.cic.flang

import Declarations._
import StateWithCats._
import cats.data.{StateT, EitherT}
import cats.implicits._

object Interpreter {
  // Define um tipo para o resultado com erro ou valor, com o erro sendo uma String
  type Result[A] = Either[String, A]
  
  // Define uma monad transformer que combina estado e tratamento de erros
  type StateEither[A] = StateT[EitherT[cats.Id, String, *], StateEnv, A]

  // Helper para levantar valores puros para StateEither
  def pure[A](a: A): StateEither[A] = StateT.pure(a)

  // Helper para levantar erros para StateEither
  def raiseError[A](msg: String): StateEither[A] = StateT.liftF(EitherT.leftT(msg))

  // Função para avaliar uma expressão e retornar um valor ou erro no contexto de StateEither
  def eval(expr: Expr): StateEither[Int] = expr match {
    case CInt(v) => pure(v)
    case Add(lhs, rhs) => for {
      l <- eval(lhs)
      r <- eval(rhs)
    } yield l + r
    case Mul(lhs, rhs) => for {
      l <- eval(lhs)
      r <- eval(rhs)
    } yield l * r
    case Id(v) => for {
      state <- StateT.get[EitherT[cats.Id, String, *], StateEnv]
      value <- lookupVar(v, state)
    } yield value
    case App(n, arg) => for {
      state <- StateT.get[EitherT[cats.Id, String, *], StateEnv]
      fdecl <- lookupFunction(n, state)
      bodyS = substitute(arg, fdecl.arg, fdecl.body)
      res <- eval(bodyS)
    } yield res
  }

  // Procura uma função no estado pelo nome, retornando uma declaração de função ou um erro
  def lookupFunction(name: String, state: StateEnv): StateEither[FDeclaration] = {
    state.collectFirst { case (n, FuncValue(d)) if n == name => d } match {
      case Some(d) => pure(d)
      case None => raiseError(s"Function $name not found")
    }
  }

  // Procura uma variável no estado pelo nome, retornando o valor ou um erro
  def lookupVar(name: String, state: StateEnv): StateEither[Int] = {
    state.collectFirst { case (n, IntValue(v)) if n == name => v } match {
      case Some(v) => pure(v)
      case None => raiseError(s"Variable $name not found")
    }
  }

  // Avalia uma expressão com declarações de funções no estado inicial
  def evalWithDeclarations(expr: Expr, declarations: List[FDeclaration]): StateEither[Int] = {
    val initialState: StateEnv = declarations.map(d => (d.name, FuncValue(d)))
    for {
      _ <- StateT.set[EitherT[cats.Id, String, *], StateEnv](initialState)
      res <- eval(expr)
    } yield res
  }

  // Executa a avaliação de uma expressão com um estado inicial fornecido, retornando o resultado ou erro e o estado final
  def run(expr: Expr, initialState: StateEnv): (Either[String, Int], StateEnv) = {
    val result = eval(expr).run(initialState).value
    result match {
      case Right((s, a)) => (Right(a), s)
      case Left(error) => (Left(error), initialState)
    }
  }
}
