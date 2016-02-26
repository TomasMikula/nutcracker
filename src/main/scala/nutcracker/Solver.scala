package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.FreeK

trait Solver[L <: Language, F[_]] {
  val lang: L

  type K[A] = FreeK[lang.Vocabulary, A]
  type S = lang.State[K]

  def assess: S => Assessment[F[K[Unit]]]
}
