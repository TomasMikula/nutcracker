package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.{FreeK, Inject, InjectK}

import scalaz.Cont

sealed trait VRef[+D]

sealed case class DRef[+D, -U, +Δ](domainId: Long) extends VRef[D]

object DRef {

  implicit def drefOps[D, U, Δ](ref: DRef[D, U, Δ]) = DRefOps(ref)

  final case class DRefOps[D, U, Δ](ref: DRef[D, U, Δ]) extends AnyVal {
    def ==>(target: DRef[D, U, Δ])(implicit inj: Inject[Meet[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
      valTriggerF(ref){ d => fireReload(meet(target)(d)) }
    def <=>(target: DRef[D, U, Δ])(implicit inj: Inject[Meet[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
      (ref ==> target) >> (target ==> ref)
    def >>=[F[_[_], _], A](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D], dom: Dom[D, U, Δ]): FreeK[F, Unit] =
      whenResolved(ref)(f)

    def asCont[F[_[_], _], A](implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): Cont[FreeK[F, Unit], A] =
      Cont { whenResolved(ref)(_) }

  }

  implicit def toCont[F[_[_], _], A, D](ref: DRef[D, _, _])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): Cont[FreeK[F, Unit], A] =
    ref.asCont
}
