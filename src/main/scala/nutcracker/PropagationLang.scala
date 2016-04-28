package nutcracker

import scala.language.higherKinds
import algebra.lattice.BoundedMeetSemilattice
import nutcracker.Dom.{Diff, Meet}
import nutcracker.util.{FreeK, FunctorK, FunctorKA, Inject, InjectK, StateInterpreterT}
import shapeless.{::, HList, HNil, Nat, Sized}

import scalaz.{Apply, Cont, Traverse, ~>}
import scalaz.std.vector._

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  case class Cell[K[_], D, U, Δ](d: D, dom: Dom[D, U, Δ]) extends PropagationLang[K, DRef[D, U, Δ]]
  case class Update[K[_], D, U, Δ](ref: DRef[D, U, Δ], u: U) extends PropagationLang[K, Unit]
  case class Fetch[K[_], D](ref: VRef[D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class DomTrigger[K[_], D, U, Δ](ref: DRef[D, U, Δ], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K]])) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K]) extends PropagationLang[K, Unit]

  // builder API for Variables
  def variable[A]: VarBuilder[A] = new VarBuilder[A]
  final class VarBuilder[A] private[PropagationLang] {
    def apply[D, U, Δ]()(implicit
      ee: EmbedExtract[A, D],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FP[DRef[D, U, Δ]] = any()
    def any[D, U, Δ]()(implicit
      ee: EmbedExtract[A, D],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FP[DRef[D, U, Δ]] = cellF(l.one)

    def oneOf(as: Set[A]): FP[CMRef[Set[A]]] = cellF(as)
    def oneOf(as: A*): FP[CMRef[Set[A]]] = oneOf(as.toSet)

    def count(n: Int): VarsBuilder[A] = new VarsBuilder(n)
  }
  final class VarsBuilder[A] private[PropagationLang](n: Int) {
    def apply[D, U, Δ]()(implicit
      ee: EmbedExtract[A, D],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FP[Vector[DRef[D, U, Δ]]] = any()
    def any[D, U, Δ]()(implicit
      ee: EmbedExtract[A, D],
      dom: Dom[D, U, Δ],
      l: BoundedMeetSemilattice[D]
    ): FP[Vector[DRef[D, U, Δ]]] = cellsF(l.one, n)

    def oneOf(as: Set[A]): FP[Vector[CMRef[Set[A]]]] = cellsF(as, n)
    def oneOf(as: A*): FP[Vector[CMRef[Set[A]]]] = oneOf(as.toSet)
  }

  // constructors returning less specific types, and curried to help with type inference
  def cell[K[_], D, U, Δ](d: D)(implicit dom: Dom[D, U, Δ]): PropagationLang[K, DRef[D, U, Δ]] = Cell(d, dom)
  def update[K[_], D, U](ref: DRef[D, U, _])(u: U): PropagationLang[K, Unit] = Update(ref, u)
  def intersect[K[_], D, U](ref: DRef[D, U, _])(d: D)(implicit inj: Inject[Meet[D], U]): PropagationLang[K, Unit] =
    update(ref)(inj(Meet(d)))
  def fetch[K[_], D](ref: VRef[D]): PropagationLang[K, D] = Fetch(ref)
  def fetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): PropagationLang[K, Sized[Vector[D], N]] = FetchVector(refs)
  def domTrigger[K[_], D, U, Δ](ref: DRef[D, U, Δ])(f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K]])): PropagationLang[K, Unit] = DomTrigger(ref, f)
  def valTrigger[K[_], D](ref: DRef[D, _, _])(f: D => Trigger[K]): PropagationLang[K, Unit] =
    domTrigger(ref)(d => f(d) match {
      case FireReload(k) => (Some(k), Some((d, δ) => f(d)))
      case Fire(k) => (Some(k), None)
      case Sleep() => (None, Some((d, δ) => f(d)))
      case Discard() => (None, None)
    })
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K]): PropagationLang[K, Unit] = SelTrigger(sel, f)
  def whenRefined[K[_], D](ref: DRef[D, _, _])(f: D => K[Unit])(implicit dom: Dom[D, _, _]): PropagationLang[K, Unit] =
    valTrigger[K, D](ref)(d => dom.assess(d) match {
      case Dom.Refined => Fire(f(d))
      case _ => Sleep()
    })
  def whenResolved[K[_], A, D](ref: DRef[D, _, _])(f: A => K[Unit])(implicit ee: EmbedExtract[A, D]): PropagationLang[K, Unit] =
    valTrigger[K, D](ref)(d => ee.extract(d) match {
      case Some(a) => Fire(f(a))
      case None => Sleep()
    })

  // constructors lifted to free programs
  def cellF[D, U, Δ](d: D)(implicit dom: Dom[D, U, Δ]): FP[DRef[D, U, Δ]] =
    FreeK.suspend(cell[FP, D, U, Δ](d))
  def cellsF[D, U, Δ](d: D, n: Int)(implicit dom: Dom[D, U, Δ]): FP[Vector[DRef[D, U, Δ]]] =
    Traverse[Vector].sequenceU(Vector.fill(n)(cellF(d)))
  def updateF[D, U](ref: DRef[D, U, _])(u: U): FP[Unit] =
    FreeK.suspend(update[FP, D, U](ref)(u))
  def intersectF[D, U](ref: DRef[D, U, _])(d: D)(implicit inj: Inject[Meet[D], U]): FP[Unit] =
    FreeK.suspend(intersect[FP, D, U](ref)(d))
  def fetchF[D](ref: VRef[D]): FP[D] =
    FreeK.suspend(fetch[FP, D](ref))
  def fetchVectorF[D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): FP[Sized[Vector[D], N]] =
    FreeK.suspend(fetchVector[FP, D, N](refs))
  def domTriggerF[F[_[_], _], D, U, Δ](ref: DRef[D, U, Δ])(f: D => (Option[FreeK[F, Unit]], Option[(D, Δ) => Trigger[FreeK[F, ?]]]))(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(domTrigger[FreeK[F, ?], D, U, Δ](ref)(f))
  def valTriggerF[F[_[_], _], D](ref: DRef[D, _, _])(f: D => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(valTrigger[FreeK[F, ?], D](ref)(f))
  def selTriggerF[F[_[_], _], L <: HList](sel: Sel[L])(f: L => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(selTrigger[FreeK[F, ?], L](sel)(f))
  def whenRefinedF[F[_[_], _], D](ref: DRef[D, _, _])(f: D => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Dom[D, _, _]): FreeK[F, Unit] =
    FreeK.lift(whenRefined[FreeK[F, ?], D](ref)(f))
  def whenResolvedF[F[_[_], _], A, D](ref: DRef[D, _, _])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): FreeK[F, Unit] =
    FreeK.lift(whenResolved[FreeK[F, ?], A, D](ref)(f))


  // convenience API
  def set[A, D, U](ref: DRef[D, U, _], a: A)(implicit ee: EmbedExtract[A, D], inj: Inject[Meet[D], U]): FP[Unit] =
    intersectF(ref)(EmbedExtract[A, D].embed(a))
  def remove[D, U, Δ](ref: DRef[D, U, Δ], d: D)(implicit inj: Inject[Diff[D], U]): FP[Unit] =
    updateF[D, U](ref)(inj(Diff(d)))
  def exclude[A, D, U, Δ](ref: DRef[D, U, Δ], a: A)(implicit ee: EmbedExtract[A, D], inj: Inject[Diff[D], U]): FP[Unit] =
    remove(ref, ee.embed(a))
  def selTrigger2[K[_], D1, D2](ref1: VRef[D1], ref2: VRef[D2])(f: (D1, D2) => Trigger[K]): PropagationLang[K, Unit] =
    selTrigger[K, D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))
  def selTrigger2F[F[_[_], _], D1, D2](ref1: VRef[D1], ref2: VRef[D2])(f: (D1, D2) => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(selTrigger2[FreeK[F, ?], D1, D2](ref1, ref2)(f))


  // Convenience API for branching as a special kind of lattice

  /** Convenience method to add an exclusive choice of multiple possibilities.
    * This is a shorthand for adding a cell whose semi-lattice is the lattice
    * of finite sets of elements of type A, initialized to the given set of
    * elements.
    */
  def branch[A](as: Set[A]): FP[CMRef[Set[A]]] = variable[A].oneOf(as)
  def branch[A](as: A*): FP[CMRef[Set[A]]] = branch(as.toSet)

  /** Convenience method to add an exclusive choice of arbitrary free programs
    * to continue. When the choice is made, the chosen program is executed.
    */
  def branchAndExec[F[_[_], _]](conts: Set[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    branch(conts) >>>= { whenResolvedF(_)((k: FreeK[F, Unit]) => k) }
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


  // Convenience API for promises as special kind of lattices

  import nutcracker.Promise._
  def promiseF[A]: FreeK[PropagationLang, Promised[A]] = cellF(Promise.empty[A])
  def completeF[A](p: Promised[A], a: A): FreeK[PropagationLang, Unit] = updateF(p)(Promise.Complete(a))

  def promiseC[F[_[_], _], A](cont: Cont[FreeK[F, Unit], A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Promised[A]] = for {
    pa <- promiseF[A].inject[F]
    _ <- cont(completeF(pa, _))
  } yield pa

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[FreeK[F, Unit], ?],
  // so we provide this API for convenience.
  def promiseC[F[_[_], _]]: PromiseContBuilder[F] = PromiseContBuilder()

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


  implicit def functorKInstance: FunctorKA[PropagationLang] = new FunctorKA[PropagationLang] {

    private def ftr[A, B, K[_], L[_]](f: (A, B) => Trigger[K], tr: K ~> L)(implicit fkt: FunctorK[Trigger]): (A, B) => Trigger[L] =
      (a, b) => fkt.transform(f(a, b))(tr)

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: K ~> L): PropagationLang[L, A] = pk match {

      // the interesting cases
      case DomTrigger(ref, f) => domTrigger(ref){ d =>
        val (now, onChange) = f(d)
        (now map (tr(_)), onChange map (action => ftr(action, tr)))
      }
      case SelTrigger(sel, f)   => selTrigger(sel){ l => FunctorK[Trigger].transform(f(l))(tr) }

      // the boring cases
      case Cell(d, dom)      => Cell(d, dom)
      case Update(ref, u)    => Update(ref, u)
      case Fetch(ref)        => Fetch(ref)
      case FetchVector(refs) => FetchVector(refs)
    }
  }

  implicit def interpreter: StateInterpreterT.StateInterpreter.Aux[PropagationLang, PropagationStore] = PropagationStore.interpreter
}