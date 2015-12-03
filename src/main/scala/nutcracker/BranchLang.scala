package nutcracker

import scala.language.higherKinds

sealed trait BranchLang[F[_], K[_], A]

object BranchLang {
  final case class BranchT[F[_], K[_], A](options: F[K[A]])

  case class AddBranching[F[_], K[_]](b: BranchT[F, K, Unit]) extends BranchLang[F, K, Unit]
}