package nutcracker.ops

import scala.language.{higherKinds, implicitConversions}

import nutcracker.{Dom, Final, FinalVars, FireReload, Join, Propagation}
import nutcracker.util.{ContU, Inject}

import scalaz.Bind
import scalaz.syntax.bind._

final case class RefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {

  def ==>[M[_]](target: Ref[D])(implicit inj: Inject[Join[D], U], P: Propagation[M, Ref]): M[Unit] =
    P.valTrigger(ref){ d => FireReload(FinalVars[M, Ref].join(target)(d)) }

  def <==[M[_]](source: Ref[D])(implicit inj: Inject[Join[D], U], P: Propagation[M, Ref]): M[Unit] =
    RefOps(source) ==> ref

  def <=>[M[_]: Bind](target: Ref[D])(implicit inj: Inject[Join[D], U], P: Propagation[M, Ref]): M[Unit] =
    (this ==> target) >> (this <== target)

  def >>=[M[_], A](f: A => M[Unit])(implicit fin: Final.Aux[D, A], P: Propagation[M, Ref]): M[Unit] =
    FinalVars[M, Ref].whenFinal(ref).exec(f)

  def asCont[M[_]](implicit fin: Final[D], P: Propagation[M, Ref]): ContU[M, fin.Out] =
    ContU { FinalVars[M, Ref].whenFinal(ref).exec(_) }

  def peekC[M[_]](implicit P: Propagation[M, Ref]): ContU[M, D] =
    ContU(f => P.peek(ref)(f))

}

trait ToRefOps {
  implicit def refOps[Ref[_], D](ref: Ref[D])(implicit dom: Dom[D]) = RefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}