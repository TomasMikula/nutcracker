package nutcracker

import nutcracker.util.Mediated
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

trait OnDemandPropagation[M[_], Ref[_]] extends Propagation[M, Ref] {
  type ExclRef[A]
  type CellCycle[A]

  /** Creates a cell that will setup itself when the first observer is registered.
    * Typically the `setup` routine starts to observe other cells.
    *
    * The `setup` routine can use the [[addFinalizer*]] method to register
    * cleanup routines (finalizers) that will be executed when all observers
    * leave. Typically, such finalizers will stop observing other cells.
    */
  def newAutoCell[A](setup: Mediated[M, A, (ExclRef[A], CellCycle[A]), Unit])(implicit dom: Dom[A]): M[Ref[A]]

  /** Register a cleanup routine to execute at the end of the cell-cycle,
    * i.e. when all of cell's observers unregister.
    * If the given cell cycle already ended, the cleanup will be executed immediately.
    *
    * @return A subscription that can be used to remove the registered finalizer early,
    *         before the end of the cell cycle. In such case, the finalizer is not executed.
    */
  def addFinalizer[A](ref: ExclRef[A], cycle: CellCycle[A], value: Subscription[M]): M[Subscription[M]]

  def update[A, U, Δ[_, _]](ref: Ref[A], cycle: CellCycle[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): M[Unit]
}