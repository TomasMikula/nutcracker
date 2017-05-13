package nutcracker.toolkit

import nutcracker.Splittable
import nutcracker.util.{FreeK, Inject}
import scalaz.Leibniz
import scalaz.Leibniz.===

sealed abstract class BranchLang[Ref[_], K[_], A] {
  import BranchLang._

  def fold[B](
    caseAddUnresolved: AddUnresolved[Ref, K, _, A] => B,
    caseRmUnresolved: RmUnresolved[Ref, K, _, A] => B,
    caseAddFailed: AddFailed[Ref, K, _, A] => B
  ): B
}

object BranchLang {
  case class AddUnresolved[Ref[_], K[_], D, A](ref: Ref[D], ev: Splittable[D], wit: Unit === A) extends BranchLang[Ref, K, A] {
    override def fold[B](caseTrack: (AddUnresolved[Ref, K, _, A]) => B, caseUntrack: (RmUnresolved[Ref, K, _, A]) => B, caseAddFailed: (AddFailed[Ref, K, _, A]) => B): B =
      caseTrack(this)
  }

  case class RmUnresolved[Ref[_], K[_], D, A](ref: Ref[D], wit: Unit === A) extends BranchLang[Ref, K, A] {
    override def fold[B](caseTrack: (AddUnresolved[Ref, K, _, A]) => B, caseUntrack: (RmUnresolved[Ref, K, _, A]) => B, caseAddFailed: (AddFailed[Ref, K, _, A]) => B): B =
      caseUntrack(this)
  }

  case class AddFailed[Ref[_], K[_], D, A](ref: Ref[D], wit: Unit === A) extends BranchLang[Ref, K, A] {
    override def fold[B](caseTrack: (AddUnresolved[Ref, K, _, A]) => B, caseUntrack: (RmUnresolved[Ref, K, _, A]) => B, caseAddFailed: (AddFailed[Ref, K, _, A]) => B): B =
      caseAddFailed(this)
  }

  def addUnresolved[Ref[_], K[_], A](ref: Ref[A])(implicit ev: Splittable[A]): BranchLang[Ref, K, Unit] = AddUnresolved(ref, ev, Leibniz.refl)
  def rmUnresolved[Ref[_], K[_], A](ref: Ref[A]): BranchLang[Ref, K, Unit] = RmUnresolved(ref, Leibniz.refl)
  def addFailed[Ref[_], K[_], A](ref: Ref[A]): BranchLang[Ref, K, Unit] = AddFailed(ref, Leibniz.refl)

  def addUnresolvedF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit ev: Splittable[A], inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(addUnresolved[Ref, FreeK[F, ?], A](ref)))
  def rmUnresolvedF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(rmUnresolved[Ref, FreeK[F, ?], A](ref)))
  def addFailedF[Ref[_], F[_[_], _], A](ref: Ref[A])(implicit inj: Inject[BranchLang[Ref, FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(addFailed[Ref, FreeK[F, ?], A](ref)))
}
