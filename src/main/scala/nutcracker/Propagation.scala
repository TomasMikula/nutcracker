package nutcracker

import scala.language.higherKinds
import scalaz.{Applicative, Functor, ~>}
import shapeless.{::, HList, HNil}
import nutcracker.util.ops.applicative._
import scalaz.Id.Id

trait Propagation[M[_], Ref[_]] extends PSrc[Ref, M] {

  // basic instructions

  def newCell[D](d: D)(implicit dom: Dom[D]): M[Ref[D]]

  def updateImpl[D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): M[Unit]

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: Id ~> λ[α => L => TriggerF[M, α]]): M[Unit]


  def newCell[D](implicit dom: DomWithBottom[D]): M[Ref[D]] =
    newCell(dom.bottom)

  def update[D](ref: Ref[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl[D, U, λ[(α, β) => Δ]](ref)(u)
  }


  // derived methods

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    newCell(d).replicate(n)

  def selTrigger2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: Id ~> λ[α => (D1, D2) => TriggerF[M, α]]): M[Unit] =
    selTrigger[D1 :: D2 :: HNil](Sel(ref1, ref2))(λ[Id ~> λ[α => (D1 :: D2 :: HNil) => TriggerF[M, α]]](
      α => l => f(α)(l.head, l.tail.head)
    ))

  def selThreshold2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: (D1, D2) => Option[M[Unit]]): M[Unit] =
    selTrigger2[D1, D2](ref1, ref2)(λ[Id ~> λ[α => (D1, D2) => TriggerF[M, α]]](α => (d1, d2) => f(d1, d2) match {
      case None => TriggerF.Sleep(α)
      case Some(mu) => TriggerF.Fire(mu)
    }))

  def _selThreshold2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: (D1, D2) => Option[M[_]])(implicit M: Functor[M]): M[Unit] =
    selThreshold2(ref1, ref2)((d1, d2) => f(d1, d2).map(M.map(_)(_ => ())))
}

object Propagation {
  def apply[M[_], Ref[_]](implicit M: Propagation[M, Ref]): Propagation[M, Ref] = M

  val module: PersistentPropagationModule = PropagationImpl
  val bundle: PropagationBundle = PropagationImpl
}

