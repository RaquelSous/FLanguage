package br.unb.cic.flang

import Declarations._
import StateWithCats._
import cats.data.State
import cats.implicits._
import Substitution._

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): M[Integer] =
    expr match {
      case CInt(v) => pure(v)
      case Add(lhs, rhs) =>
        bind(eval(lhs, declarations))({ l =>
          bind(eval(rhs, declarations))({ r => pure(l + r) })
        })
      case Mul(lhs, rhs) =>
        bind(eval(lhs, declarations))({ l =>
          bind(eval(rhs, declarations))({ r => pure(l * r) })
        })
      // case Id(name) => bind(get())({ state => pure(lookupVar(name, state)) })
      case App(name, arg) => {
        val fdecl = lookup(name, declarations)
        bind(eval(arg, declarations))({ value =>
          bind(get())({ s1 =>
            bind(put(declareVar(fdecl.arg, value, s1)))({ s2 =>
              eval(fdecl.body, declarations)
            })
          })
        })
      }
      // ifThenElse
      // booleanos: 
      case CTrue() => pure(1) // True is any non 0 value
      case CFalse() => pure(0)
      // boolean operators
      case Not(e) => eval(e, declarations).map(b => if (b == 0) 1 else 0)
      
      case And(lhs, rhs) => for {
        l <- eval(lhs, declarations)
        r <- eval(rhs, declarations)
      } yield if (l != 0 && r != 0) 1 else 0

      case Or(lhs, rhs) => for {
        l <- eval(lhs, declarations)
        r <- eval(rhs, declarations)
      } yield if (l != 0 || r != 0) 1 else 0

      case CEquals(lhs, rhs) => for {
        l <- eval(lhs, declarations)
        r <- eval(rhs, declarations)
      } yield if (l == r) 1 else 0

      // condicional
      case IfThenElse(cond, ifTrue, ifFalse) => 
        bind(eval(cond, declarations))({ c =>
          if(c != 0) eval(ifTrue, declarations)
          else eval(ifFalse, declarations)
        })
    }
    
    // stateWithCats
     def eval(expr: Expr): StateM[Integer] = expr match {
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
      state <- get
      value = lookupVar(v, state)
    } yield value
    case App(n, arg) => for {
      state <- get
      fdecl = lookupFunction(n, state)
      bodyS = substitute(arg, fdecl.arg, fdecl.body)
      res <- eval(bodyS)
    } yield res
  }

  def lookupFunction(name: String, state: S): FDeclaration = {
    state.collectFirst { case (n, FuncValue(d)) if n == name => d }
      .getOrElse(throw new RuntimeException(s"Function $name not found"))
  }

  def evalWithDeclarations(expr: Expr, declarations: List[FDeclaration]): StateM[Integer] = {
    val initialState: S = declarations.map(d => (d.name, FuncValue(d)))
    for {
      _ <- put(initialState)
      res <- eval(expr)
    } yield res
  }

  def run(expr: Expr, initialState: S): (Integer, S) = {
    runState(eval(expr), initialState)
  }
}
