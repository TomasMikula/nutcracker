import scala.language.higherKinds

import algebra.Eq
import algebra.lattice.BoundedMeetSemilattice
import nutcracker.DecSet.DecSetRef
import nutcracker.PropagationLang
import nutcracker.PropagationLang._
import nutcracker.lib.bool.{BoolDomain, BoolRef}
import nutcracker.lib.bool.BoolDomain._
import nutcracker.util.{FreeK, Inject, InjectK}
import scalaz.{Apply, Cont, Traverse, \/}

package object nutcracker {

  /** Used as a monotonic update on domain D:
    * represents the operation of meet with the given value.
    */
  final case class Meet[+D](value: D) extends AnyVal

  /** When used as a monotonic update, represents the operation of relative complement.
    * When used as a delta, represents the part that was removed.
    * An operation `Diff(d)` applied to value `d0` will result in the new value being
    * `d0 \ d` and the published delta being `Diff(d0 ∧ d)`.
    */
  final case class Diff[+D](value: D) extends AnyVal

  type Promised[A] = DRef[Promise[A], Promise.Complete[A], Unit]

  def concat[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
    ps.foldLeft[FreeK[F, Unit]](FreeK.pure(())) { _ >> _ }

  def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
    Traverse[C].sequence[FreeK[F, ?], A](ps)

  def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
    Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)


  /* **************************************************** *
   * Convenience API to work with domains that in the end *
   * resolve to a value of a different ("smaller") type.  *
   * **************************************************** */

  def variable[A]: VarBuilder[A] = new VarBuilder[A]
  final class VarBuilder[A] private[nutcracker] {
    def apply[D, U, Δ]()(implicit
      ex: Extract.Aux[D, A],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FreeK[PropagationLang, DRef[D, U, Δ]] = any()
    def any[D, U, Δ]()(implicit
      ex: Extract.Aux[D, A],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FreeK[PropagationLang, DRef[D, U, Δ]] = cellF(l.one)

    def oneOf(as: Set[A]): FreeK[PropagationLang, DecSetRef[A]] = cellF(DecSet.wrap(as))
    def oneOf(as: A*): FreeK[PropagationLang, DecSetRef[A]] = oneOf(as.toSet)

    def count(n: Int): VarsBuilder[A] = new VarsBuilder(n)
  }

  final class VarsBuilder[A] private[nutcracker](n: Int) {
    def apply[D, U, Δ]()(implicit
      ee: Extract.Aux[D, A],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FreeK[PropagationLang, Vector[DRef[D, U, Δ]]] = any()
    def any[D, U, Δ]()(implicit
      ee: Extract.Aux[D, A],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FreeK[PropagationLang, Vector[DRef[D, U, Δ]]] = cellsF(l.one, n)

    def oneOf(as: Set[A]): FreeK[PropagationLang, Vector[DecSetRef[A]]] = cellsF(DecSet.wrap(as), n)
    def oneOf(as: A*): FreeK[PropagationLang, Vector[DecSetRef[A]]] = oneOf(as.toSet)
  }

  def whenResolved[F[_[_], _], A, D](ref: DRef[D, _, _])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ex: Extract.Aux[D, A]): FreeK[F, Unit] =
    valTriggerF[F, D](ref)(d => ex.extract(d) match {
      case Some(a) => Fire[FreeK[F, ?]](f(a))
      case None => Sleep[FreeK[F, ?]]()
    })


  /* ****************************************************** *
   * Convenience methods to work with lattice-based domains *
   * ****************************************************** */

  def meet[D, U, Δ](ref: DRef[D, U, Δ])(d: D)(implicit inj: Inject[Meet[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
    updateF(ref)(inj(Meet(d)))

  def set[A, D, U, Δ](ref: DRef[D, U, Δ], a: A)(implicit em: Embed[A, D], inj: Inject[Meet[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
    meet(ref)(em.embed(a))

  def remove[D, U, Δ](ref: DRef[D, U, Δ], d: D)(implicit inj: Inject[Diff[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
    updateF[D, U, Δ](ref)(inj(Diff(d)))

  def exclude[A, D, U, Δ](ref: DRef[D, U, Δ], a: A)(implicit em: Embed[A, D], inj: Inject[Diff[D], U], dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
    remove(ref, em.embed(a))

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
  ): FreeK[PropagationLang, BoolRef] =
    for {
      res <- variable[Boolean]()
      _ <- whenResolved(res) { (r: Boolean) => if(r) different(d1, d2) else d1 <=> d2 }
      _ <- whenRefinedF(d1) { r1 => whenRefinedF(d2) { r2 => set[Boolean, BoolDomain, Meet[BoolDomain] \/ Diff[BoolDomain], Unit](res, Eq[D].neqv(r1, r2)) } }
    } yield res


  /* ***************************************** *
   * Convenience methods to work with promises *
   * ***************************************** */

  def promise[A]: FreeK[PropagationLang, Promised[A]] = cellF(Promise.empty[A])
  def complete[A](p: Promised[A], a: A): FreeK[PropagationLang, Unit] = updateF(p)(Promise.Complete(a))

  def promiseC[F[_[_], _], A](cont: Cont[FreeK[F, Unit], A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[A]] = for {
    pa <- promise[A].inject[F]
    _ <- cont(complete(pa, _))
  } yield pa

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[FreeK[F, Unit], ?],
  // so we provide this API for convenience.
  def promiseC[F[_[_], _]]: PromiseContBuilder[F] = PromiseContBuilder()

  def promiseResults[D, U, Δ](cells: Vector[DRef[D, U, Δ]])(implicit ex: Extract[D]): FreeK[PropagationLang, Promised[Vector[ex.Out]]] = {

    def go(pr: Promised[Vector[ex.Out]], tail: List[ex.Out], i: Int): FreeK[PropagationLang, Unit] = {
      if(i < 0) {
        complete(pr, tail.toVector)
      } else {
        whenResolved(cells(i))((a: ex.Out) => go(pr, a :: tail, i-1))(implicitly, ex)
      }
    }

    for {
      pr <- promise[Vector[ex.Out]]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[D, U, Δ](cells: DRef[D, U, Δ]*)(implicit ex: Extract[D]): FreeK[PropagationLang, Promised[Vector[ex.Out]]] =
    promiseResults(cells.toVector)


  /* ********************************************************** *
   * Convenience API for branching as a special kind of lattice *
   * ********************************************************** */

  /** Convenience method to add an exclusive choice of multiple possibilities.
    * This is a shorthand for adding a cell whose semi-lattice is the lattice
    * of finite sets of elements of type A, initialized to the given set of
    * elements.
    */
  def branch[A](as: Set[A]): FreeK[PropagationLang, DecSetRef[A]] = variable[A].oneOf(as)
  def branch[A](as: A*): FreeK[PropagationLang, DecSetRef[A]] = branch(as.toSet)

  /** Convenience method to add an exclusive choice of arbitrary free programs
    * to continue. When the choice is made, the chosen program is executed.
    */
  def branchAndExec[F[_[_], _]](conts: Set[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    branch(conts) >>>= { whenResolved(_)((k: FreeK[F, Unit]) => k) }
  def branchAndExec[F[_[_], _]](conts: FreeK[F, Unit]*)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    branchAndExec(conts.toSet)

  /** Convenience method to add an exclusive choice of multiple possibilities,
    * presented as a continuation of the chosen element. Note that a "branching
    * cell" (see [[branch(Set[A])]]) is added for each callback that is registered
    * on the returned continuation. Thus, if two callback are registered on the
    * returned continuation, it will have the effect of making a choice from the
    * cartesian product `as × as`. If this is not what you want, use
    * [[branch(Set[A])]] directly.
    */
  def branchC[A, F[_[_], _]](as: Set[A])(implicit inj: InjectK[PropagationLang, F]): Cont[FreeK[F, Unit], A] =
    Cont(f => branch(as) >>>= { _.asCont.apply(f) })
  def branchC[A, F[_[_], _]](as: A*)(implicit inj: InjectK[PropagationLang, F]): Cont[FreeK[F, Unit], A] =
    branchC(as.toSet)
}


import nutcracker._

case class PromiseContBuilder[F[_[_], _]]() {
  private type Kont[A] = Cont[FreeK[F, Unit], A]
  private val A = Apply[Kont]

  def tuple[A1, A2](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[(A1, A2)]] =
    promiseC(A.tuple2(a1, a2))
  def tuple[A1, A2, A3](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[(A1, A2, A3)]] =
    promiseC(A.tuple3(a1, a2, a3))
  def tuple[A1, A2, A3, A4](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[(A1, A2, A3, A4)]] =
    promiseC(A.tuple4(a1, a2, a3, a4))
  def tuple[A1, A2, A3, A4, A5](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[(A1, A2, A3, A4, A5)]] =
    promiseC(A.tuple5(a1, a2, a3, a4, a5))

  def apply[A1, A2, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(f: (A1, A2) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[R]] =
    promiseC(A.apply2(a1, a2)(f))
  def apply[A1, A2, A3, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(f: (A1, A2, A3) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[R]] =
    promiseC(A.apply3(a1, a2, a3)(f))
  def apply[A1, A2, A3, A4, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(f: (A1, A2, A3, A4) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[R]] =
    promiseC(A.apply4(a1, a2, a3, a4)(f))
  def apply[A1, A2, A3, A4, A5, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(f: (A1, A2, A3, A4, A5) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[R]] =
    promiseC(A.apply5(a1, a2, a3, a4, a5)(f))
}