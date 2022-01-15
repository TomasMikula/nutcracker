package nutcracker.data

import nutcracker.ops.Ops._
import nutcracker.util.{Cont, Id}
import nutcracker.{Dom, Final, Propagation}
import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.bind._
import scalaz.{Apply, Bind, Equal}

/**
  * Convenience methods to work with promises.
  */
object Promises {

  def promise[A]: PromiseBuilder[A] =
    PromiseBuilder[A]

  private def promise[M[_], Var[_], A: Equal](implicit M: Propagation.Aux0[M, Var]): M[Var[Promise[A]]] =
    M.newCell(Promise.empty[A])

  def complete[M[_], Var[_], A: Equal](p: Var[Promise[A]], a: A)(using M: Propagation.Aux0[M, Var]): M[Unit] =
    M.update(p).by(Promise.Completed(a))

  extension [M[_], A: Equal](using P: Propagation[M])(pa: P.Var[Promise[A]]) {
    def complete(a: A): M[Unit] =
      Promises.complete(pa, a)
  }

  def promiseC[M[_], Var[_], A: Equal](cont: Cont[M[Unit], A])(implicit P: Propagation.Aux0[M, Var]): M[Var[Promise[A]]] = {
    import P.M
    promise[A]() >>= (pa => M.map(cont(Id(a => Id(complete[M, Var, A](pa, a)))).value)((_: Unit) => pa))
  }

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[M[Unit], ?],
  // so we provide this API for convenience.
  def promiseC[M[_], Var[_]](implicit M: Propagation.Aux0[M, Var]): PromiseContBuilder[M, Var] =
    new PromiseContBuilder

  def promiseResults[M[_], Var[_], D, A](cells: Vector[Var[D]])(implicit
    P: Propagation.Aux0[M, Var],
    fin: Final.Aux[D, A],
    dom: Dom[D],
    EqA: Equal[A],
  ): M[Var[Promise[Vector[fin.Out]]]] = {
    import P.M

    def go(pr: Var[Promise[Vector[fin.Out]]], tail: List[fin.Out], i: Int): M[Unit] = {
      if (i < 0) {
        complete(pr, tail.toVector)
      } else {
        cells(i).whenFinal_((a: fin.Out) => go(pr, a :: tail, i - 1))
      }
    }

    for {
      pr <- promise[Vector[fin.Out]]()
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[M[_], Var[_], D, A](cells: Var[D]*)(implicit
    M: Propagation.Aux0[M, Var],
    fin: Final.Aux[D, A],
    dom: Dom[D],
    EqA: Equal[A],
  ): M[Var[Promise[Vector[fin.Out]]]] =
    promiseResults(cells.toVector)

  final class PromiseBuilder[A] private() {
    def apply[M[_]]()(using P: Propagation[M], A: Equal[A]): M[P.Var[Promise[A]]] =
      promise[M, P.Var, A]
  }
  object PromiseBuilder {
    private val instance: PromiseBuilder[Any] = new PromiseBuilder

    def apply[A]: PromiseBuilder[A] = instance.asInstanceOf[PromiseBuilder[A]]
  }

  final class PromiseContBuilder[M[_], Var[_]](implicit P: Propagation.Aux0[M, Var]) {
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