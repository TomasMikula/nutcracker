import scala.language.higherKinds

import algebra.Eq
import cats.std.vector._
import nutcracker.PropagationLang._
import nutcracker.lib.bool.BoolDomain
import nutcracker.lib.bool.BoolDomain._
import nutcracker.util.free._

import algebra.lattice.GenBool
import scalaz.Traverse

package object nutcracker {

  type Promised[A] = DomRef[A, nutcracker.Promise[A]]

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.Pure(())) { _ >> _ }

  def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
    Traverse[C].sequence[FreeK[F, ?], A](ps)

  def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
    Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)

  def different[A, D: Domain[A, ?] : GenBool](d1: DomRef[A, D], d2: DomRef[A, D]): FreeK[PropagationLang, Unit] = {
    whenResolvedF(d1){ a => remove(d2, a) } >>
    whenResolvedF(d2){ a => remove(d1, a) }
  }

  def allDifferent[A, D: Domain[A, ?] : GenBool](doms: DomRef[A, D]*): FreeK[PropagationLang, Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenResolvedF(doms(i)){ a =>
      concat((0 until i) map { j => remove(doms(j), a) }) >>
      concat((i+1 until n) map { j => remove(doms(j), a) }) }
    })
  }

  def isDifferent[A: Eq, D: Domain[A, ?] : GenBool](d1: DomRef[A, D], d2: DomRef[A, D]): FreeK[PropagationLang, DomRef[Boolean, BoolDomain]] =
    for {
      res <- variable[Boolean]()
      _ <- whenResolvedF(res) { if(_) different(d1, d2) else d1 <=> d2 }
      _ <- whenResolvedF(d1) { a1 => whenResolvedF(d2) { a2 => set(res, Eq[A].neqv(a1, a2)) } }
    } yield res

  def promiseResults[A: Eq, D](cells: Vector[DomRef[A, D]]): FreeK[PropagationLang, Promised[Vector[A]]] = {

    def go(pr: Promised[Vector[A]], tail: List[A], i: Int): FreeK[PropagationLang, Unit] = {
      if(i < 0) {
        completeF(pr, tail.toVector)
      } else {
        whenResolvedF(cells(i)){ a => go(pr, a :: tail, i-1) }
      }
    }

    for {
      pr <- promiseF[Vector[A]]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[A: Eq, D](cells: DomRef[A, D]*): FreeK[PropagationLang, Promised[Vector[A]]] =
    promiseResults(cells.toVector)
}
