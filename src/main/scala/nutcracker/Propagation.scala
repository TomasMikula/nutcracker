package nutcracker

import nutcracker.util.{Exists, IndexedContT}
import nutcracker.util.ops.applicative._
import scala.language.implicitConversions
import scalaz.Monad
import scalaz.syntax.functor._

trait Propagation[M[_]] extends Observe[M] {
  import Propagation.IUpdateRes

  type IVar[A[_]]

  type Out[_]

  type Var[A] = IVar[[i] =>> A]

  override implicit def M: Monad[M]

  def iReadOnly[D[_]](a: IVar[D]): IVal[D]

  implicit def readOnly[A](ref: Var[A]): Val[A] =
    iReadOnly[[i] =>> A](ref)

  def newICell[D[_], I](d: D[I])(implicit dom: IDom[D]): M[IVar[D]]

  final def newCell[D](d: D)(implicit dom: Dom[D]): M[Var[D]] =
    newICell[[i] =>> D, Any](d)

  def iUpdate[D[_], U[_], J](ref: IVar[D])(u: U[J])(implicit dom: IDom.Aux0[D, U]): M[IUpdateRes[D, dom.IChange, J, ?]]

  final def updateImpl[D, U, Δ](ref: Var[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): M[Unit] =
    iUpdate[[i] =>> D, [j] =>> U, Any](ref)(u)(dom).void


  def newCell[D](implicit dom: DomWithBottom[D]): M[Var[D]] =
    newCell(dom.bottom)

  def update[D](ref: Var[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Var[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl[D, U, Δ](ref)(u)
  }

  def cells[D](d: D, n: Int)(implicit dom: Dom[D]): M[Vector[Var[D]]] =
    newCell(d).replicate(n)

  def iOut[D[_], B](v: IVar[D], f: [i] => D[i] => B): Out[B]

  def iOut[D[_]](v: IVar[D]): Out[Exists[D]] =
    iOut[D, Exists[D]](v, [i] => (d: D[i]) => Exists(d))

  def out[D](v: Var[D]): Out[D] =
    iOut[[i] =>> D, D](v, [i] => (d: D) => d)

  def constOut[A](a: A): Out[A]

  def pairOut[A, B](a: Out[A], b: Out[B]): Out[(A, B)]

  def mapOut[A, B](a: Out[A])(f: A => B): Out[B]

  def flatMapOut[A, B](a: Out[A])(f: A => Out[B]): Out[B]

  implicit def monadOut: Monad[Out] =
    new Monad[Out] {
      override def point[A](a: => A): Out[A] =
        constOut(a)

      override def bind[A, B](a: Out[A])(f: A => Out[B]): Out[B] =
        flatMapOut(a)(f)
    }
}

object Propagation {
  type Aux0[M[_], Var0[_]] =
    Propagation[M] { type Var[A] = Var0[A] }

  type Aux1[M[_], Var[_], Val0[_]] =
    Propagation.Aux0[M, Var] { type Val[A] = Val0[A] }

  type Aux[M[_], Var[_], Val[_], Out0[_]] =
    Propagation.Aux1[M, Var, Val] { type Out[A] = Out0[A] }

  def apply[M[_], Ref[_], Val[_], Out[_]](implicit M: Propagation.Aux[M, Ref, Val, Out]): Propagation.Aux[M, Ref, Val, Out] = M

  sealed trait IUpdateRes[D[_], Δ[_, _, _], J, K]
  object IUpdateRes {
    case class Updated[D[_], Δ[_, _, _], I, J, K](delta: Δ[I, J, K], newValue: D[K]) extends IUpdateRes[D, Δ, J, K]
    case class Unchanged[D[_], Δ[_, _, _], J, K](value: D[K]) extends IUpdateRes[D, Δ, J, K]
  }
}

trait OnDemandPropagation[M[_]] extends Propagation[M] {
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

  def exclUpdateImpl[A, U, Δ](ref: ExclRef[A], u: U)(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def exclUpdate[D](ref: ExclRef[D])(implicit dom: Dom[D]): ExclUpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new ExclUpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class ExclUpdateSyntaxHelper[D, U, Δ](ref: ExclRef[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = exclUpdateImpl[D, U, Δ](ref, u)
  }
}

object OnDemandPropagation {
  type Aux1[M[_], Var0[_], Val0[_]] = OnDemandPropagation[M] with Propagation.Aux1[M, Var0, Val0]

  type Aux[M[_], Var0[_], Val0[_], Out0[_]] = OnDemandPropagation.Aux1[M, Var0, Val0] { type Out[A] = Out0[A] }
}