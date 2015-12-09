import scala.language.higherKinds

import nutcracker.PromiseLang._
import nutcracker.PropagationLang._
import nutcracker.util.free._

import algebra.lattice.GenBool
import scalaz.Traverse

package object nutcracker {

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.Pure(())) { _ >> _ }

  def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
    Traverse[C].sequence[FreeK[F, ?], A](ps)

  def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
    Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)

  def allDifferent[A, D: Domain[A, ?] : GenBool](doms: DomRef[A, D]*): FreeK[PropagationLang, Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenResolvedF(doms(i)){ a =>
      concat((0 until i) map { j => remove(doms(j), a) }) >>
      concat((i+1 until n) map { j => remove(doms(j), a) }) }
    })
  }

  def promiseResult[A, D](cell: DomRef[A, D]): FreeK[CoproductK[PropagationLang, PromiseLang, ?[_], ?], Promised[A]] = {
    type PP[K[_], T] = CoproductK[PropagationLang, PromiseLang, K, T]
    for {
      prA <- promiseF[A].inj[PP]
      _ <- whenResolvedF[PP, A, D](cell){ a => completeF(prA, a) }
    } yield prA
  }

  def promiseResults[A, D](cells: Vector[DomRef[A, D]]): FreeK[CoproductK[PropagationLang, PromiseLang, ?[_], ?], Promised[Vector[A]]] = {
    type PP[K[_], T] = CoproductK[PropagationLang, PromiseLang, K, T]

    def go(pr: Promised[Vector[A]], tail: List[A], i: Int): FreeK[PP, Unit] = {
      if(i < 0) {
        completeF(pr, tail.toVector).inj[PP]
      } else {
        whenResolvedF(cells(i)){ a => go(pr, a :: tail, i-1) }
      }
    }

    for {
      pr <- promiseF[Vector[A]].inj[PP]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[A, D](cells: DomRef[A, D]*): FreeK[CoproductK[PropagationLang, PromiseLang, ?[_], ?], Promised[Vector[A]]] =
    promiseResults(cells.toVector)
}
