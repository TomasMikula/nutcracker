package nutcracker.toolkit

import nutcracker.Splittable
import nutcracker.util.{FreeK, Inject}
import scalaz.Leibniz
import scalaz.Leibniz.===

sealed abstract class BranchLang[Ref[_], K[_], A] {
  import BranchLang._

  def fold[B](
    caseTrack: Track[Ref, K, _, A] => B,
    caseUntrack: Untrack[Ref, K, _, A] => B
  ): B
}

object BranchLang {
  case class Track[Ref[_], K[_], D, A](ref: Ref[D], ev: Splittable[D], wit: Unit === A) extends BranchLang[Ref, K, A] {
    override def fold[B](caseTrack: (Track[Ref, K, _, A]) => B, caseUntrack: (Untrack[Ref, K, _, A]) => B): B = caseTrack(this)
  }

  case class Untrack[Ref[_], K[_], D, A](ref: Ref[D], wit: Unit === A) extends BranchLang[Ref, K, A] {
    override def fold[B](caseTrack: (Track[Ref, K, _, A]) => B, caseUntrack: (Untrack[Ref, K, _, A]) => B): B = caseUntrack(this)
  }

  def track[Ref[_], K[_], A](ref: Ref[A])(implicit ev: Splittable[A]): BranchLang[Ref, K, Unit] = Track(ref, ev, Leibniz.refl)
  def untrack[Ref[_], K[_], A](ref: Ref[A]): BranchLang[Ref, K, Unit] = Untrack(ref, Leibniz.refl)

  def trackF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit ev: Splittable[A], inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(track[Ref, FreeK[F, ?], A](ref)))
  def untrackF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(untrack[Ref, FreeK[F, ?], A](ref)))
}
