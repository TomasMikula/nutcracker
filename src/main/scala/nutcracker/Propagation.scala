package nutcracker

import nutcracker.util.ops.applicative._
import scala.language.implicitConversions
import scalaz.{Applicative, Functor, IndexedContT}
import scalaz.syntax.functor._
import shapeless.{::, HList, HNil}

trait Propagation[M[_], Var[_], Val[_]] extends PSrc[Val, M] {

  implicit def readOnly[A](a: Var[A]): Val[A]

  // basic instructions

  def newCell[D](d: D)(implicit dom: Dom[D]): M[Var[D]]

  def updateImpl[D, U, Δ[_, _]](ref: Var[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): M[Unit]

  def selTrigger[L <: HList](sel: Sel[Var, L])(f: L => (Option[M[Unit]], Boolean)): M[Unit]


  def newCell[D](implicit dom: DomWithBottom[D]): M[Var[D]] =
    newCell(dom.bottom)

  def update[D](ref: Var[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Var[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl[D, U, λ[(α, β) => Δ]](ref)(u)
  }


  // derived methods

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Var[D]]] =
    newCell(d).replicate(n)

  def selTrigger2[D1, D2](ref1: Var[D1], ref2: Var[D2])(f: (D1, D2) => (Option[M[Unit]], Boolean)): M[Unit] =
    selTrigger[D1 :: D2 :: HNil](Sel(ref1, ref2))((l: D1 :: D2 :: HNil) => f(l.head, l.tail.head))

  def selThreshold2[D1, D2](ref1: Var[D1], ref2: Var[D2])(f: (D1, D2) => Option[M[Unit]]): M[Unit] =
    selTrigger2[D1, D2](ref1, ref2)((d1, d2) => f(d1, d2) match {
      case None => (None, true)
      case Some(mu) => (Some(mu), false)
    })

  def _selThreshold2[D1, D2](ref1: Var[D1], ref2: Var[D2])(f: (D1, D2) => Option[M[_]])(implicit M: Functor[M]): M[Unit] =
    selThreshold2(ref1, ref2)((d1, d2) => f(d1, d2).map(_.void))
}

object Propagation {
  def apply[M[_], Ref[_], Val[_]](implicit M: Propagation[M, Ref, Val]): Propagation[M, Ref, Val] = M

  val module: PersistentPropagationModule = PropagationImpl
  val bundle: PropagationBundle = PropagationImpl
}

trait OnDemandPropagation[M[_], Var[_], Val[_]] extends Propagation[M, Var, Val] {
  type ExclRef[A]

  /** Creates a cell that will setup itself when the first observer is registered.
    * Typically the `setup` routine starts to observe other cells.
    *
    * The `setup` routine can use the [[addFinalizer*]] method to register
    * cleanup routines (finalizers) that will be executed when all observers
    * leave. Typically, such finalizers will stop observing other cells.
    */
  def newAutoCell[A](setup: IndexedContT[M, Unit, ExclRef[A], A])(implicit dom: Dom[A]): M[Val[A]]

  /** Register a cleanup routine to execute at the end of the cell-cycle,
    * i.e. when all of cell's observers unregister.
    * If the cycle in which the given cell was created already ended,
    * the cleanup will be executed immediately.
    *
    * @return A subscription that can be used to remove the registered finalizer early,
    *         before the end of the cell cycle. In such case, the finalizer is not executed.
    */
  def addFinalizer[A](ref: ExclRef[A], value: Subscription[M]): M[Subscription[M]]

  def update[A, U, Δ[_, _]](ref: ExclRef[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): M[Unit]
}