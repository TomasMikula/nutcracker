package nutcracker

import scala.language.higherKinds

sealed trait BranchLang[F[_], K[_], A]

object BranchLang {
  case class AddBranching[F[_], K[_]](b: F[K[Unit]]) extends BranchLang[F, K, Unit]
}