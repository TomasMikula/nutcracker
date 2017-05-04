package nutcracker.toolkit

import nutcracker.Splittable
import nutcracker.util.{FreeK, Inject}

sealed abstract class BranchLang[Ref[_], K[_], A]

object BranchLang {
  case class Track[Ref[_], K[_], A](ref: Ref[A], ev: Splittable[A]) extends BranchLang[Ref, K, Unit]
  case class Untrack[Ref[_], K[_], A](ref: Ref[A]) extends BranchLang[Ref, K, Unit]

  def track[Ref[_], K[_], A](ref: Ref[A])(implicit ev: Splittable[A]): BranchLang[Ref, K, Unit] = Track(ref, ev)
  def untrack[Ref[_], K[_], A](ref: Ref[A]): BranchLang[Ref, K, Unit] = Untrack(ref)

  def trackF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit ev: Splittable[A], inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(track[Ref, FreeK[F, ?], A](ref)))
  def untrackF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(untrack[Ref, FreeK[F, ?], A](ref)))
}