package nutcracker

import scala.language.higherKinds

import nutcracker.ops._
import scalaz.{Apply, Bind, Cont, Equal}
import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.bind._

/**
  * Convenience methods to work with promises.
  */
final class PromiseOps[M[_], Ref[_]](implicit M: Propagation[M, Ref]) {

  def promise[A: Equal]: M[Ref[Promise[A]]] = M.newCell(Promise.empty[A])

  def complete[A: Equal](p: Ref[Promise[A]], a: A): M[Unit] = M.update(p).by(Promise.Completed(a))

  def promiseC[A: Equal](cont: Cont[M[Unit], A])(implicit MB: Bind[M]): M[Ref[Promise[A]]] =
    promise[A] >>= (pa => MB.map(cont(complete(pa, _)))((_: Unit) => pa))

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[M[Unit], ?],
  // so we provide this API for convenience.
  def promiseC(implicit MB: Bind[M]): PromiseContBuilder = new PromiseContBuilder()

  def promiseResults[D, A](cells: Vector[Ref[D]])(implicit fin: Final.Aux[D, A], dom: Dom[D], EqA: Equal[A], MB: Bind[M]): M[Ref[Promise[Vector[fin.Out]]]] = {

    def go(pr: Ref[Promise[Vector[fin.Out]]], tail: List[fin.Out], i: Int): M[Unit] = {
      if (i < 0) {
        complete(pr, tail.toVector)
      } else {
        cells(i).whenFinal(a => go(pr, a :: tail, i - 1))
      }
    }

    for {
      pr <- promise[Vector[fin.Out]]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[D, A](cells: Ref[D]*)(implicit fin: Final.Aux[D, A], dom: Dom[D], EqA: Equal[A], MB: Bind[M]): M[Ref[Promise[Vector[fin.Out]]]] =
    promiseResults(cells.toVector)

  final class PromiseContBuilder()(implicit MB: Bind[M]) {
    private type Kont[A] = Cont[M[Unit], A]
    private val A = Apply[Kont]

    def tuple[A1: Equal, A2: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2]): M[Ref[Promise[(A1, A2)]]] =
      promiseC(A.tuple2(a1, a2))
    def tuple[A1: Equal, A2: Equal, A3: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3]): M[Ref[Promise[(A1, A2, A3)]]] =
      promiseC(A.tuple3(a1, a2, a3))
    def tuple[A1: Equal, A2: Equal, A3: Equal, A4: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4]): M[Ref[Promise[(A1, A2, A3, A4)]]] =
      promiseC(A.tuple4(a1, a2, a3, a4))
    def tuple[A1: Equal, A2: Equal, A3: Equal, A4: Equal, A5: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4], a5: Cont[M[Unit], A5]): M[Ref[Promise[(A1, A2, A3, A4, A5)]]] =
      promiseC(A.tuple5(a1, a2, a3, a4, a5))

    def apply[A1, A2, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2])(f: (A1, A2) => R): M[Ref[Promise[R]]] =
      promiseC(A.apply2(a1, a2)(f))
    def apply[A1, A2, A3, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3])(f: (A1, A2, A3) => R): M[Ref[Promise[R]]] =
      promiseC(A.apply3(a1, a2, a3)(f))
    def apply[A1, A2, A3, A4, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4])(f: (A1, A2, A3, A4) => R): M[Ref[Promise[R]]] =
      promiseC(A.apply4(a1, a2, a3, a4)(f))
    def apply[A1, A2, A3, A4, A5, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4], a5: Cont[M[Unit], A5])(f: (A1, A2, A3, A4, A5) => R): M[Ref[Promise[R]]] =
      promiseC(A.apply5(a1, a2, a3, a4, a5)(f))
  }
}

object PromiseOps {
  def apply[M[_], Ref[_]](implicit P: Propagation[M, Ref]): PromiseOps[M, Ref] = new PromiseOps[M, Ref]
}