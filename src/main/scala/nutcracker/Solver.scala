package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.FreeK
import scalaz.{Functor, Monoid}

trait Solver[L <: Language, F[_]] {
  val lang: L

  type K[A] = FreeK[lang.Vocabulary, A]
  type S = lang.State[K]

  def emptyState: S = lang.emptyState[K]

  def assess(state: S): Assessment[F[(S, K[Unit])]]

  implicit def vocabularyFunctor: Functor[lang.Vocabulary[K, ?]] = lang.vocabularyFunctor[K]

  implicit def dirtyMonoid: Monoid[lang.Dirty[K]] = lang.dirtyMonoidK.monoid[K]
}