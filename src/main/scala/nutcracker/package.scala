import scala.language.higherKinds
import nutcracker.PropagationLang._
import nutcracker.lib.bool.BoolRef
import nutcracker.lib.bool.BoolDomain._
import nutcracker.util.{ContF, FreeK, FreeKT, Inject, InjectK}

import scalaz.{Apply, Cont}
import scalaz.Id._

package nutcracker {

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice meet with the given value.
    */
  final case class Meet[+D](value: D) extends AnyVal

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice join with the given value.
    */
  final case class Join[D](value: D) extends AnyVal

  /** When used as a monotonic update, represents the operation of relative complement.
    * When used as a delta, represents the part that was removed.
    * An operation `Diff(d)` applied to value `d0` will result in the new value being
    * `d0 \ d` and the published delta being `Diff(d0 ∧ d)`.
    */
  final case class Diff[+D](value: D) extends AnyVal


  final class VarBuilder[A] private[nutcracker] {
    def apply[D]()(implicit
      ex: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): FreeK[PropagationLang, DRef.Aux[D, dom.Update, dom.Delta]] = any()
    def any[D]()(implicit
      ex: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): FreeK[PropagationLang, DRef.Aux[D, dom.Update, dom.Delta]] = cellF(dom.bottom)

    def oneOf(as: Set[A]): FreeK[PropagationLang, DecSet.Ref[A]] = cellF(DecSet.wrap(as))
    def oneOf(as: A*): FreeK[PropagationLang, DecSet.Ref[A]] = oneOf(as.toSet)

    def count(n: Int): VarsBuilder[A] = new VarsBuilder(n)
  }

  final class VarsBuilder[A] private[nutcracker](n: Int) {
    def apply[D]()(implicit
      ee: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): FreeK[PropagationLang, Vector[DRef.Aux[D, dom.Update, dom.Delta]]] = any()
    def any[D]()(implicit
      ee: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): FreeK[PropagationLang, Vector[DRef.Aux[D, dom.Update, dom.Delta]]] = cellsF(dom.bottom, n)

    def oneOf(as: Set[A]): FreeK[PropagationLang, Vector[DecSet.Ref[A]]] = cellsF(DecSet.wrap(as), n)
    def oneOf(as: A*): FreeK[PropagationLang, Vector[DecSet.Ref[A]]] = oneOf(as.toSet)
  }

  final case class WhenFinal[D, A] private[nutcracker](ref: DRef[D], fin: Final.Aux[D, A]) {
    def exec[F[_[_], _]](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
      valTriggerF[F, D](ref)(d => fin.extract(d) match {
        case Some(a) => Fire[FreeK[F, Unit]](f(a))
        case None => Sleep[FreeK[F, Unit]]()
      })

    def exec0[F[_[_], _]](f: D => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
      valTriggerF[F, D](ref)(d =>
        if(fin.isFinal(d)) Fire[FreeK[F, Unit]](f(d))
        else Sleep[FreeK[F, Unit]]()
      )
  }

}

package object nutcracker {

  /* **************************************************** *
   * Convenience API to work with domains that in the end *
   * resolve to a value of a different ("smaller") type,  *
   * which cannot be refined further without reaching a   *
   * contradiction (thus is final).                       *
   * **************************************************** */

  def variable[A]: VarBuilder[A] = new VarBuilder[A]

  def whenFinal[D](ref: DRef[D])(implicit fin: Final[D]): WhenFinal[D, fin.Out] =
    WhenFinal[D, fin.Out](ref, fin)


  /* ****************************************************** *
   * Convenience methods to work with lattice-based domains *
   * ****************************************************** */

  def meet[D](ref: DRef[D])(d: D)(implicit inj: Inject[Meet[D], ref.Update], dom: Dom.Aux[D, ref.Update, ref.Delta]): FreeK[PropagationLang, Unit] =
    updateF(ref)(inj(Meet(d)))

  def set[A, D](ref: DRef[D], a: A)(implicit fin: Final.Aux[D, A], inj: Inject[Meet[D], ref.Update], dom: Dom.Aux[D, ref.Update, ref.Delta]): FreeK[PropagationLang, Unit] =
    meet(ref)(fin.embed(a))

  def remove[D](ref: DRef[D], d: D)(implicit inj: Inject[Diff[D], ref.Update], dom: Dom.Aux[D, ref.Update, ref.Delta]): FreeK[PropagationLang, Unit] =
    updateF[D](ref)(inj(Diff(d)))

  def exclude[A, D](ref: DRef[D], a: A)(implicit fin: Final.Aux[D, A], inj: Inject[Diff[D], ref.Update], dom: Dom.Aux[D, ref.Update, ref.Delta]): FreeK[PropagationLang, Unit] =
    remove(ref, fin.embed(a))

  def different[D, U, Δ](d1: DRef.Aux[D, U, Δ], d2: DRef.Aux[D, U, Δ])(implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    inj: Inject[Diff[D], U]
  ): FreeK[PropagationLang, Unit] = {
    whenFinal(d1).exec0(d => remove(d2, d)) >>
    whenFinal(d2).exec0(d => remove(d1, d))
  }

  def allDifferent[D, U, Δ](doms: DRef.Aux[D, U, Δ]*)(implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    inj: Inject[Diff[D], U]
  ): FreeK[PropagationLang, Unit] = {
    val n = doms.size
    FreeK.sequence_((0 until n) map { i => whenFinal(doms(i)).exec0(d =>
      FreeK.sequence_((0 until i) map { j => remove(doms(j), d) }) >>
      FreeK.sequence_((i+1 until n) map { j => remove(doms(j), d) })
    )})
  }

  def isDifferent[D, U, Δ](d1: DRef.Aux[D, U, Δ], d2: DRef.Aux[D, U, Δ])(implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    injd: Inject[Diff[D], U],
    injm: Inject[Meet[D], U]
  ): FreeK[PropagationLang, BoolRef] =
    for {
      res <- variable[Boolean]()
      _ <- whenFinal(res).exec(r => if(r) different(d1, d2) else d1 <=> d2)
      _ <- whenFinal(d1).exec0(r1 => whenFinal(d2).exec0(r2 => {
        val r = dom.update(r1, injm(Meet(r2))) match {
          case None => false
          case Some((x, _)) => dom.assess(x) == Dom.Failed
        }
        set(res, r)
      }))
    } yield res


  /* ***************************************** *
   * Convenience methods to work with promises *
   * ***************************************** */

  def promise[A]: FreeKT[PropagationLang, Id, Promise.Ref[A]] = cellF(Promise.empty[A])
  def complete[A](p: Promise.Ref[A], a: A): FreeK[PropagationLang, Unit] = updateF(p)(Promise.Complete(a))

  def promiseC[F[_[_], _], A](cont: Cont[FreeK[F, Unit], A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[A]] = for {
    pa <- promise[A].inject[F]
    _ <- cont(complete(pa, _))
  } yield pa

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[FreeK[F, Unit], ?],
  // so we provide this API for convenience.
  def promiseC[F[_[_], _]]: PromiseContBuilder[F] = PromiseContBuilder()

  def promiseResults[D](cells: Vector[DRef[D]])(implicit fin: Final[D]): FreeK[PropagationLang, Promise.Ref[Vector[fin.Out]]] = {

    def go(pr: Promise.Ref[Vector[fin.Out]], tail: List[fin.Out], i: Int): FreeK[PropagationLang, Unit] = {
      if(i < 0) {
        complete(pr, tail.toVector)
      } else {
        whenFinal(cells(i)).exec(a => go(pr, a :: tail, i-1))
      }
    }

    for {
      pr <- promise[Vector[fin.Out]]
      _ <- go(pr, Nil, cells.size - 1)
    } yield pr
  }

  def promiseResults[D](cells: DRef[D]*)(implicit fin: Final[D]): FreeK[PropagationLang, Promise.Ref[Vector[fin.Out]]] =
    promiseResults(cells.toVector)


  /* ********************************************************** *
   * Convenience API for branching as a special kind of lattice *
   * ********************************************************** */

  /** Convenience method to add an exclusive choice of multiple possibilities.
    * This is a shorthand for adding a cell whose semi-lattice is the lattice
    * of finite sets of elements of type A, initialized to the given set of
    * elements.
    */
  def branch[A](as: Set[A]): FreeK[PropagationLang, DecSet.Ref[A]] = variable[A].oneOf(as)
  def branch[A](as: A*): FreeK[PropagationLang, DecSet.Ref[A]] = branch(as.toSet)

  /** Convenience method to add an exclusive choice of arbitrary free programs
    * to continue. When the choice is made, the chosen program is executed.
    */
  def branchAndExec[F[_[_], _]](conts: Set[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    branch(conts) >>>= { whenFinal(_).exec(k => k) }
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
  def branchC[A, F[_[_], _]](as: Set[A])(implicit inj: InjectK[PropagationLang, F]): ContF[F, A] =
    ContF(f => branch(as) >>>= { _.asCont.apply(f) })
  def branchC[A, F[_[_], _]](as: A*)(implicit inj: InjectK[PropagationLang, F]): ContF[F, A] =
    branchC(as.toSet)
}


import nutcracker._

case class PromiseContBuilder[F[_[_], _]]() {
  private type Kont[A] = Cont[FreeK[F, Unit], A]
  private val A = Apply[Kont]

  def tuple[A1, A2](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[(A1, A2)]] =
    promiseC(A.tuple2(a1, a2))
  def tuple[A1, A2, A3](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[(A1, A2, A3)]] =
    promiseC(A.tuple3(a1, a2, a3))
  def tuple[A1, A2, A3, A4](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[(A1, A2, A3, A4)]] =
    promiseC(A.tuple4(a1, a2, a3, a4))
  def tuple[A1, A2, A3, A4, A5](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[(A1, A2, A3, A4, A5)]] =
    promiseC(A.tuple5(a1, a2, a3, a4, a5))

  def apply[A1, A2, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(f: (A1, A2) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[R]] =
    promiseC(A.apply2(a1, a2)(f))
  def apply[A1, A2, A3, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(f: (A1, A2, A3) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[R]] =
    promiseC(A.apply3(a1, a2, a3)(f))
  def apply[A1, A2, A3, A4, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(f: (A1, A2, A3, A4) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[R]] =
    promiseC(A.apply4(a1, a2, a3, a4)(f))
  def apply[A1, A2, A3, A4, A5, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(f: (A1, A2, A3, A4, A5) => R)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promise.Ref[R]] =
    promiseC(A.apply5(a1, a2, a3, a4, a5)(f))
}
