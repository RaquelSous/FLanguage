import br.unb.cic.flang._
import cats.data._
import cats.implicits._

object MErr {
  type MError[A] = EitherT[State[S, *], String, A]

  object eh {
    def raiseError[A](msg: String): MError[A] = EitherT.leftT(msg)
    def pure[A](a: A): MError[A] = EitherT.rightT(a)
  }
}

object Example2 extends App {
  // Definição de funções
  val declarations = List(
    FDeclaration("addOne", "x", Add(Id("x"), CInt(1))),
    FDeclaration("multiplyByTwo", "x", Mul(Id("x"), CInt(2)))
  )

  type S = List[(String, StateValue)]

  // Estado inicial como lista de declarações
  val initialState: S = declarations.map(d => (d.name, FuncValue(d)))

  // Função para buscar e executar uma função no estado atual
  def executeFunction(name: String): MError[Integer] = for {
    fDecl <- Declarations.lookup(name, declarations)
    result <- Interpreter.eval(fDecl.body) // Avalia o corpo da função com um argumento fictício
  } yield result

  // Executa uma função existente
  val (stateAfterRun1, res1) = executeFunction("addOne").value.run(initialState).value
  println(res1) // Deveria imprimir: Right(1)

  // Tenta executar uma função inexistente
  val (stateAfterRun2, res2) = executeFunction("subtract").value.run(initialState).value
  println(res2) // Deveria imprimir: Left(Function subtract is not declared)
}
