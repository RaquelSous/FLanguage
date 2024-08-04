import br.unb.cic.flang._
import MErr._
import MErr.eh._
import cats.implicits._

object Example1 extends App {
  // Definição de algumas funções
  val declarations = List(
    FDeclaration("addOne", "x", Add(Id("x"), CInt(1))),
    FDeclaration("multiplyByTwo", "x", Mul(Id("x"), CInt(2)))
  )

  // Busca por uma função existente
  val result1: MError[FDeclaration] = Declarations.lookup("addOne", declarations)
  println(result1) // Deveria imprimir: Right(FDeclaration(addOne,x,Add(Id(x),CInt(1))))

  // Busca por uma função inexistente
  val result2: MError[FDeclaration] = Declarations.lookup("subtract", declarations)
  println(result2) // Deveria imprimir: Left(Function subtract is not declared)
}
