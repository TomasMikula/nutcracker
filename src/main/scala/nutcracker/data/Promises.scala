package nutcracker.data

import nutcracker.ops.Ops._
import nutcracker.{Dom, Final, Propagation}
import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.bind._
import scalaz.{Apply, Bind, Cont, Equal}

/**
  * Convenience methods to work with promises.
  */
object Promises {

  def promise[A]: PromiseBuilder[A] =
    PromiseBuilder[A]

  private def promise[M[_], Var[_], Val[_], A: Equal](implicit M: Propagation[M, Var, Val]): M[Var[Promise[A]]] =
    M.newCell(Promise.empty[A])

  def complete[M[_], Var[_], Val[_], A: Equal](p: Var[Promise[A]], a: A)(implicit M: Propagation[M, Var, Val]): M[Unit] =
    M.update(p).by(Promise.Completed(a))

  def promiseC[M[_], Var[_], Val[_], A: Equal](cont: Cont[M[Unit], A])(implicit M: Propagation[M, Var, Val], MB: Bind[M]): M[Var[Promise[A]]] =
    promise[A]() >>= (pa => MB.map(cont(complete(pa, _)))((_: Unit) => pa))

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[M[Unit], ?],
  // so we provide this API for convenience.
  def promiseC[M[_], Var[_], Val[_]](implicit M: Propagation[M, Var, Val], MB: Bind[M]): PromiseContBuilder[M, Var, Val] =
    new PromiseContBuilder

  def promiseResults[M[_], Var[_], Val[_], D, A](cells: Vector[Var[D]])(implicit M: Propagation[M, Var, Val], fin: Final.Aux[D, A], dom: Dom[D], EqA: Equal[A], MB: Bind[M]): M[Var[Promise[Vector[fin.Out]]]] = {

    def go(pr: Var[Promise[Vector[fin.Out]]], tail: List[fin.Out], i: Int): M[Unit] = {
      if (i < 0) {
        complete(pr, tail.toVector)
      } else {
        cells(i).whenFinal_(a => go(pr, a :: tail, i - 1))
      }
    }

    for {
      pr <- promise[Vector[fin.Out]]()
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[M[_], Var[_], Val[_], D, A](cells: Var[D]*)(implicit M: Propagation[M, Var, Val], fin: Final.Aux[D, A], dom: Dom[D], EqA: Equal[A], MB: Bind[M]): M[Var[Promise[Vector[fin.Out]]]] =
    promiseResults(cells.toVector)

  final class PromiseBuilder[A] private() {
    def apply[M[_], Var[_], Val[_]]()(implicit M: Propagation[M, Var, Val], A: Equal[A]): M[Var[Promise[A]]] =
      promise[M, Var, Val, A]
  }
  object PromiseBuilder {
    private val instance: PromiseBuilder[Any] = new PromiseBuilder

    def apply[A]: PromiseBuilder[A] = instance.asInstanceOf[PromiseBuilder[A]]
  }

  final class PromiseContBuilder[M[_], Var[_], Val[_]](implicit P: Propagation[M, Var, Val], MB: Bind[M]) {
    private type Kont[A] = Cont[M[Unit], A]
    private val A = Apply[Kont]

    def tuple[A1: Equal, A2: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2]): M[Var[Promise[(A1, A2)]]] =
      promiseC(A.tuple2(a1, a2))
    def tuple[A1: Equal, A2: Equal, A3: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3]): M[Var[Promise[(A1, A2, A3)]]] =
      promiseC(A.tuple3(a1, a2, a3))
    def tuple[A1: Equal, A2: Equal, A3: Equal, A4: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4]): M[Var[Promise[(A1, A2, A3, A4)]]] =
      promiseC(A.tuple4(a1, a2, a3, a4))
    def tuple[A1: Equal, A2: Equal, A3: Equal, A4: Equal, A5: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4], a5: Cont[M[Unit], A5]): M[Var[Promise[(A1, A2, A3, A4, A5)]]] =
      promiseC(A.tuple5(a1, a2, a3, a4, a5))

    def apply[A1, A2, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2])(f: (A1, A2) => R): M[Var[Promise[R]]] =
      promiseC(A.apply2(a1, a2)(f))
    def apply[A1, A2, A3, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3])(f: (A1, A2, A3) => R): M[Var[Promise[R]]] =
      promiseC(A.apply3(a1, a2, a3)(f))
    def apply[A1, A2, A3, A4, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4])(f: (A1, A2, A3, A4) => R): M[Var[Promise[R]]] =
      promiseC(A.apply4(a1, a2, a3, a4)(f))
    def apply[A1, A2, A3, A4, A5, R: Equal](a1: Cont[M[Unit], A1], a2: Cont[M[Unit], A2], a3: Cont[M[Unit], A3], a4: Cont[M[Unit], A4], a5: Cont[M[Unit], A5])(f: (A1, A2, A3, A4, A5) => R): M[Var[Promise[R]]] =
      promiseC(A.apply5(a1, a2, a3, a4, a5)(f))
  }
}