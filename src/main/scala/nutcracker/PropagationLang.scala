package nutcracker

import scala.language.higherKinds

import algebra.lattice.{BoundedMeetSemilattice, GenBool}
import nutcracker.util.{FreeK, FunctorK, FunctorKA, InjectK, StateInterpreterT}
import shapeless.{::, HNil, Sized, Nat, HList}
import scalaz._
import scalaz.std.vector._

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  case class Variable[K[_], A, D](d: D, dom: Domain[A, D]) extends PropagationLang[K, LRef[D]]
  case class Update[K[_], D, U, Δ](ref: DRef[D, U, Δ], u: U) extends PropagationLang[K, Unit]
  case class Fetch[K[_], D](ref: VRef[D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class VarTrigger[K[_], D](ref: VRef[D], f: D => Trigger[K]) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K]) extends PropagationLang[K, Unit]

  // builder API for Variables
  def variable[A]: VarBuilder[A] = new VarBuilder[A]
  final class VarBuilder[A] private[PropagationLang] {
    def apply[D: Domain[A, ?] : BoundedMeetSemilattice](): FP[LRef[D]] = any()
    def any[D: Domain[A, ?] : BoundedMeetSemilattice](): FP[LRef[D]] = init(BoundedMeetSemilattice[D].one)
    def init[D: Domain[A, ?]](d: D): FP[LRef[D]] = FreeK.suspend(Variable[FP, A, D](d, implicitly[Domain[A, D]]))

    def oneOf(as: Set[A]): FP[LRef[Set[A]]] = FreeK.suspend(Variable[FP, A, Set[A]](as, implicitly[Domain[A, Set[A]]]))
    def oneOf(as: A*): FP[LRef[Set[A]]] = oneOf(as.toSet)

    def count(n: Int): VarsBuilder[A] = new VarsBuilder(n)
  }
  final class VarsBuilder[A] private[PropagationLang](n: Int) {
    def apply[D: Domain[A, ?] : BoundedMeetSemilattice](): FP[Vector[LRef[D]]] = any()
    def any[D: Domain[A, ?] : BoundedMeetSemilattice](): FP[Vector[LRef[D]]] = init[D](BoundedMeetSemilattice[D].one)
    def init[D: Domain[A, ?]](d: D): FP[Vector[LRef[D]]] =
      Traverse[Vector].sequenceU(Vector.fill(n)(variable[A].init(d)))

    def oneOf(as: Set[A]): FP[Vector[LRef[Set[A]]]] = init(as)
    def oneOf(as: A*): FP[Vector[LRef[Set[A]]]] = oneOf(as.toSet)
  }

  // constructors returning less specific types, and curried to help with type inference
  def update[K[_], D, U](ref: DRef[D, U, _])(u: U): PropagationLang[K, Unit] = Update(ref, u)
  def intersect[K[_], D](ref: LRef[D])(d: D): PropagationLang[K, Unit] = update(ref)(d)
  def fetch[K[_], D](ref: VRef[D]): PropagationLang[K, D] = Fetch(ref)
  def fetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): PropagationLang[K, Sized[Vector[D], N]] = FetchVector(refs)
  def varTrigger[K[_], D](ref: VRef[D])(f: D => Trigger[K]): PropagationLang[K, Unit] = VarTrigger(ref, f)
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K]): PropagationLang[K, Unit] = SelTrigger(sel, f)
  def whenResolved[K[_], A, D](ref: VRef[D])(f: A => K[Unit])(implicit dom: Domain[A, D]): PropagationLang[K, Unit] =
    varTrigger[K, D](ref)(d => dom.values(d) match {
      case Domain.Empty() => Discard()
      case Domain.Just(a) => Fire(f(a))
      case Domain.Many(_) => Sleep()
    })

  // constructors lifted to free programs
  def updateF[D, U](ref: DRef[D, U, _])(u: U): FP[Unit] =
    FreeK.suspend(update[FP, D, U](ref)(u))
  def intersectF[D](ref: LRef[D])(d: D): FP[Unit] =
    FreeK.suspend(intersect[FP, D](ref)(d))
  def fetchF[D](ref: VRef[D]): FP[D] =
    FreeK.suspend(fetch[FP, D](ref))
  def fetchVectorF[D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): FP[Sized[Vector[D], N]] =
    FreeK.suspend(fetchVector[FP, D, N](refs))
  def varTriggerF[F[_[_], _], D](ref: DRef[D, _, _])(f: D => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(varTrigger[FreeK[F, ?], D](ref)(f))
  def selTriggerF[F[_[_], _], L <: HList](sel: Sel[L])(f: L => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(selTrigger[FreeK[F, ?], L](sel)(f))
  def whenResolvedF[F[_[_], _], A, D](ref: VRef[D])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): FreeK[F, Unit] =
    FreeK.lift(whenResolved[FreeK[F, ?], A, D](ref)(f))


  // convenience API
  def set[A, D: Domain[A, ?]](ref: LRef[D], a: A): FP[Unit] =
    intersectF(ref)(Domain[A, D].singleton(a))
  def remove[A, D: Domain[A, ?] : GenBool](ref: LRef[D], a: A): FP[Unit] = {
    val d = Domain[A, D].singleton(a)
    fetchF(ref) >>= { d0 => intersectF(ref)(GenBool[D].without(d0, d)) }
  }
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
  def branch[A](as: Set[A]): FP[LRef[Set[A]]] = variable[A].oneOf(as)
  def branch[A](as: A*): FP[LRef[Set[A]]] = branch(as.toSet)

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
  def promiseF[A]: FreeK[PropagationLang, Promised[A]] = variable[A].any[nutcracker.Promise[A]]
  def completeF[A](p: Promised[A], a: A): FreeK[PropagationLang, Unit] = set[A, nutcracker.Promise[A]](p, a)

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

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: K ~> L): PropagationLang[L, A] = pk match {

      // the interesting cases
      case VarTrigger(ref, f)   => varTrigger(ref){ d => FunctorK[Trigger].transform(f(d))(tr) }
      case SelTrigger(sel, f)   => selTrigger(sel){ l => FunctorK[Trigger].transform(f(l))(tr) }

      // the boring cases
      case Variable(d, dom)  => Variable(d, dom)
      case Update(ref, u)    => Update(ref, u)
      case Fetch(ref)        => Fetch(ref)
      case FetchVector(refs) => FetchVector(refs)
    }
  }

  implicit def interpreter: StateInterpreterT.StateInterpreter.Aux[PropagationLang, PropagationStore] = PropagationStore.interpreter
}