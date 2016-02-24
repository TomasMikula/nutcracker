package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.FreeK

trait Solver[L <: Language, F[_]] {
  val lang: L

  type K[A] = FreeK[lang.Vocabulary, A]
  type S = lang.State[K]

  def emptyState: S = lang.emptyState[K]

  def assess(state: S): Assessment[F[(S, K[Unit])]]
}
