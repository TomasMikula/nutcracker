import scala.language.higherKinds
import algebra.Eq
import nutcracker.PropagationLang._
import nutcracker.lib.bool.BoolDomain
import nutcracker.lib.bool.BoolDomain._
import nutcracker.util.{FreeK, Inject}
import nutcracker.Dom.{Diff, Meet, Res}

import scalaz.{Traverse, \/}

package object nutcracker {

  /** Monotonic update of a Complemented Meet lattice:
    * either meet or diff (relative complement)
    */
  type CMUpdate[D] = Meet[D] \/ Diff[D]

  /** Published changes to a Complemented Meet lattice:
    * either the new value (Res), or the difference to previous value (Diff).
    */
  type CMDelta[D] = Res[D] \/ Diff[D]

  type CMRef[D] = DRef[D, CMUpdate[D], CMDelta[D]]
  type CMURef[D] = DRef[D, CMUpdate[D], Unit]

  type Promised[A] = DRef[Promise[A], Promise.Complete[A], Unit]

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.Pure(())) { _ >> _ }

  def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
    Traverse[C].sequence[FreeK[F, ?], A](ps)

  def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
    Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)

  def different[D, U, Δ](d1: DRef[D, U, Δ], d2: DRef[D, U, Δ])(implicit
    dom: Dom[D, U, Δ],
    inj: Inject[Diff[D], U]
  ): FreeK[PropagationLang, Unit] = {
    whenRefinedF(d1){ d => remove(d2, d) } >>
    whenRefinedF(d2){ d => remove(d1, d) }
  }

  def allDifferent[D, U, Δ](doms: DRef[D, U, Δ]*)(implicit
    dom: Dom[D, U, Δ],
    inj: Inject[Diff[D], U]
  ): FreeK[PropagationLang, Unit] = {
    val n = doms.size
    concat((0 until n) map { i => whenRefinedF(doms(i)){ d =>
      concat((0 until i) map { j => remove(doms(j), d) }) >>
      concat((i+1 until n) map { j => remove(doms(j), d) }) }
    })
  }

  def isDifferent[D: Eq, U, Δ](d1: DRef[D, U, Δ], d2: DRef[D, U, Δ])(implicit
    dom: Dom[D, U, Δ],
    injd: Inject[Diff[D], U],
    injm: Inject[Meet[D], U]
  ): FreeK[PropagationLang, CMURef[BoolDomain]] =
    for {
      res <- variable[Boolean]()
      _ <- whenResolvedF(res) { (r: Boolean) => if(r) different(d1, d2) else d1 <=> d2 }
      _ <- whenRefinedF(d1) { r1 => whenRefinedF(d2) { r2 => set[Boolean, BoolDomain, Meet[BoolDomain] \/ Diff[BoolDomain]](res, Eq[D].neqv(r1, r2)) } }
    } yield res

  def promiseResults[A, D, U, Δ](cells: Vector[DRef[D, U, Δ]])(implicit ee: EmbedExtract[A, D]): FreeK[PropagationLang, Promised[Vector[A]]] = {

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

  def promiseResults[A, D, U, Δ](cells: DRef[D, U, Δ]*)(implicit ee: EmbedExtract[A, D]): FreeK[PropagationLang, Promised[Vector[A]]] =
    promiseResults(cells.toVector)
}
