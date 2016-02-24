package nutcracker

import nutcracker.util.free.Interpreter

import scala.language.higherKinds

trait Language {
  type Vocabulary[K[_], A]
  type State[K[_]]
  type Dirty[K[_]]

  val interpreter: Interpreter.Aux[Vocabulary, State, Dirty]
}
