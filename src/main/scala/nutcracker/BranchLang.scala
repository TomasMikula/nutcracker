package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.{InjectK, FreeK}

sealed trait BranchLang[B[_], K[_], A]

object BranchLang {
  case class AddBranching[B[_], K[_]](b: B[K[Unit]]) extends BranchLang[B, K, Unit]

  def addBranching[B[_], K[_]](b: B[K[Unit]]): BranchLang[B, K, Unit] = AddBranching(b)

  def addBranchingF[B[_], F[_[_], _]](b: B[FreeK[F, Unit]])(implicit inj: InjectK[BranchLang[B, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.lift[BranchLang[B, ?[_], ?], F, Unit](addBranching[B, FreeK[F, ?]](b))
}