package br.unb.cic.flang

import cats.data.{EitherT, State}
import cats.implicits._

// Define o tipo de estado (S) que será usado em todo o projeto
type S = List[(String, StateValue)]

object MErr {
  // MError é um tipo que combina State e Either, com uma mensagem de erro do tipo String
  type MError[A] = EitherT[State[S, *], String, A]

  object eh {
    // Função para criar um erro no contexto de MError
    def raiseError[A](msg: String): MError[A] = EitherT.leftT(msg)

    // Função para criar um valor de sucesso no contexto de MError
    def pure[A](a: A): MError[A] = EitherT.rightT(a)
  }
}
