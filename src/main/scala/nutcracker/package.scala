import scala.language.higherKinds

import algebra.Eq
import nutcracker.PropagationLang._
import nutcracker.lib.bool.BoolDomain
import nutcracker.lib.bool.BoolDomain._
import nutcracker.util.FreeK

import algebra.lattice.GenBool
import scalaz.Traverse

package object nutcracker {

  type LRef[D] = DRef[D, D, D]

  type Promised[A] = LRef[nutcracker.Promise[A]]

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.Pure(())) { _ >> _ }

  def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
    Traverse[C].sequence[FreeK[F, ?], A](ps)

  def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
    Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)

  def different[A, D: Domain[A, ?] : GenBool](d1: LRef[D], d2: LRef[D]): FreeK[PropagationLang, Unit] = {
    whenResolvedF(d1){ (a: A) => remove(d2, a) } >>
    whenResolvedF(d2){ (a: A) => remove(d1, a) }
  }

  def allDifferent[A, D: Domain[A, ?] : GenBool](doms: LRef[D]*): FreeK[PropagationLang, Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenResolvedF(doms(i)){ (a: A) =>
      concat((0 until i) map { j => remove(doms(j), a) }) >>
      concat((i+1 until n) map { j => remove(doms(j), a) }) }
    })
  }

  def isDifferent[A: Eq, D: Domain[A, ?] : GenBool](d1: LRef[D], d2: LRef[D]): FreeK[PropagationLang, LRef[BoolDomain]] =
    for {
      res <- variable[Boolean]()
      _ <- whenResolvedF(res) { (r: Boolean) => if(r) different(d1, d2) else d1 <=> d2 }
      _ <- whenResolvedF(d1) { (a1: A) => whenResolvedF(d2) { (a2: A) => set(res, Eq[A].neqv(a1, a2)) } }
    } yield res

  def promiseResults[A, D](cells: Vector[VRef[D]])(implicit dom: Domain[A, D]): FreeK[PropagationLang, Promised[Vector[A]]] = {

    def go(pr: Promised[Vector[A]], tail: List[A], i: Int): FreeK[PropagationLang, Unit] = {
      if(i < 0) {
        completeF(pr, tail.toVector)
      } else {
        whenResolvedF(cells(i)){ (a: A) => go(pr, a :: tail, i-1) }
      }
    }

    for {
      pr <- promiseF[Vector[A]]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[A, D](cells: VRef[D]*)(implicit dom: Domain[A, D]): FreeK[PropagationLang, Promised[Vector[A]]] =
    promiseResults(cells.toVector)
}
