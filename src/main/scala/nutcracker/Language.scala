package nutcracker

import nutcracker.util.free.{MonoidK, Interpreter}

import scala.language.higherKinds
import scalaz.Functor

trait Language {
  type Vocabulary[K[_], A]
  type State[K[_]]
  type Dirty[K[_]]

  val interpreter: Interpreter[Vocabulary, State, Dirty]

  def emptyState[K[_]]: State[K]

  def vocabularyFunctor[K[_]]: Functor[Vocabulary[K, ?]]
  def dirtyMonoidK: MonoidK[Dirty]
}
