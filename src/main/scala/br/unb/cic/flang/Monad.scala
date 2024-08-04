package br.unb.cic.flang

import cats.data.State
import cats.implicits._

package object StateWithCats {
  type StateEnv = List[(String, StateValue)]

  // Define a State Monad, parametrizado pelo tipo de ambiente e o tipo de valor de retorno
  type StateM[A] = State[StateEnv, A]

  // Cria um valor puro na monad de estado
  def pure[A](a: A): StateM[A] = State.pure(a)

  // Combinador bind (flatMap) para sequenciamento de operações na monad de estado
  def bind[A, B](m: StateM[A])(f: A => StateM[B]): StateM[B] = m.flatMap(f)

  // Define o estado atual
  def put(state: StateEnv): StateM[Unit] = State.set(state)

  // Obtém o estado atual
  def get: StateM[StateEnv] = State.get

  // Modifica o estado atual aplicando uma função
  def modify(f: StateEnv => StateEnv): StateM[Unit] = State.modify(f)

  // Executa uma computação de estado com um estado inicial, retornando o resultado e o estado final
  def runState[A](state: StateM[A], initial: StateEnv): (A, StateEnv) = state.run(initial).value

  // Declara uma variável no estado com um nome e valor inicial
  def declareVar(name: String, value: Int, state: StateEnv): StateEnv =
    (name, IntValue(value)) :: state

  // Procura o valor de uma variável no estado; lança uma exceção se não for encontrada
  def lookupVar(name: String, state: StateEnv): Int = state match {
    case Nil => throw new RuntimeException(s"Variable $name not found")
    case (n, IntValue(v)) :: tail if n == name => v
    case _ :: tail => lookupVar(name, tail)
  }
}
