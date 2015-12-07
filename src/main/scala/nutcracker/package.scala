import scala.language.higherKinds

import nutcracker.PropagationLang._
import nutcracker.util.free._
import nutcracker.util.free.Interpreter._
import nutcracker.util.free.PromiseLang._

import algebra.lattice.GenBool
import scalaz.Id._
import scalaz.StreamT

package object nutcracker {

  type Assessor[S, U] = S => Assessment[U]
  type Advancer[S, U, F[_], K[_]] = (S, U) => F[(S, K[Unit])]

  type AssessAdvance[S, F[_], K[_]] = S => Assessment[F[(S, K[Unit])]]

  type BranchL[K[_], A] = BranchLang[StreamT[Id, ?], K, A]
  type BranchS[K[_]] = BranchStore[StreamT[Id, ?], K]

  type Lang0[K[_], A] = CoproductK[BranchL, PromiseLang, K, A]
  type Lang1[K[_], A] = CoproductK[PropagationLang, Lang0, K, A]
  type Lang[K[_], A] = CoyonedaK[Lang1, K, A]

  type Store0[K[_]] = ProductK[BranchS, PromiseStore, K]
  type Store[K[_]] = ProductK[Domains, Store0, K]

  type Dirty0[K[_]] = ProductK[AlwaysClean, AlwaysClean, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, Dirty0, K]

  type Program[A] = FreeK[Lang, A]

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.Pure(())) { _ >> _ }

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
}
