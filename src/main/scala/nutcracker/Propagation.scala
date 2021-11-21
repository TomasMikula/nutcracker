package nutcracker

import nutcracker.util.ops.applicative._
import scala.language.implicitConversions
import scalaz.{Applicative, IndexedContT}

trait Propagation[M[_], Var[_], Val0[_]] extends Observe[M] {
  type Val[A] = Val0[A]

  implicit def readOnly[A](a: Var[A]): Val[A]

  def newCell[D](d: D)(implicit dom: Dom[D]): M[Var[D]]

  def updateImpl[D, U, Δ[_, _]](ref: Var[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): M[Unit]


  def newCell[D](implicit dom: DomWithBottom[D]): M[Var[D]] =
    newCell(dom.bottom)

  def update[D](ref: Var[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Var[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl[D, U, λ[(α, β) => Δ]](ref)(u)
  }

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Var[D]]] =
    newCell(d).replicate(n)
}

object Propagation {
  def apply[M[_], Ref[_], Val[_]](implicit M: Propagation[M, Ref, Val]): Propagation[M, Ref, Val] = M
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
  def newAutoCellC[A](setup: IndexedContT[Unit, ExclRef[A], M, A])(implicit dom: Dom[A]): M[Val[A]]

  def newAutoCell[A](setup: (A => M[ExclRef[A]]) => M[Unit])(implicit dom: Dom[A]): M[Val[A]] =
    newAutoCellC[A](IndexedContT(setup))

  /** Register a cleanup routine to execute at the end of the cell-cycle,
    * i.e. when all of cell's observers unregister.
    * If the cycle in which the given cell was created already ended,
    * the cleanup will be executed immediately.
    *
    * @return A subscription that can be used to remove the registered finalizer early,
    *         before the end of the cell cycle. In such case, the finalizer is not executed.
    */
  def addFinalizer[A](ref: ExclRef[A], value: Subscription[M]): M[Subscription[M]]

  def exclUpdateImpl[A, U, Δ[_, _]](ref: ExclRef[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): M[Unit]

  def exclUpdate[D](ref: ExclRef[D])(implicit dom: Dom[D]): ExclUpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new ExclUpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class ExclUpdateSyntaxHelper[D, U, Δ](ref: ExclRef[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = exclUpdateImpl[D, U, λ[(α, β) => Δ]](ref, u)
  }
}