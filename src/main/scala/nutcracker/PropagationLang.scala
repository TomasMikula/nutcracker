package nutcracker

import scala.language.higherKinds

import algebra.lattice.{BoundedLattice, GenBool}
import nutcracker.util.free.{InjectK, FreeK, FunctorK, FunctorKA}

import shapeless.{::, HNil, Sized, Nat, HList}
import scalaz.{Traverse, ~>}
import scalaz.std.vector._

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  case class Variable[K[_], A, D](d: D, dom: Domain[A, D]) extends PropagationLang[K, DomRef[A, D]]
  case class Intersect[K[_], D](ref: CellRef[D], d: D) extends PropagationLang[K, Unit]
  case class IntersectVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]) extends PropagationLang[K, Unit]
  case class Fetch[K[_], D](ref: CellRef[D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class VarTrigger[K[_], D](ref: CellRef[D], f: D => Trigger[K]) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K]) extends PropagationLang[K, Unit]
  case class WhenResolved[K[_], A, D](ref: DomRef[A, D], f: A => K[Unit]) extends PropagationLang[K, Unit]

  // builder API for Variables
  def variable[A]: VarBuilder[A] = new VarBuilder[A]
  final class VarBuilder[A] private[PropagationLang] {
    def apply[D: Domain[A, ?] : BoundedLattice](): FP[DomRef[A, D]] = apply(BoundedLattice[D].one)
    def apply[D: Domain[A, ?]](d: D): FP[DomRef[A, D]] = lift(Variable[FP, A, D](d, implicitly[Domain[A, D]]))

    def oneOf(s: Set[A]): FP[DomRef[A, Set[A]]] = lift(Variable[FP, A, Set[A]](s, implicitly[Domain[A, Set[A]]]))

    def count(n: Int): VarsBuilder[A] = new VarsBuilder(n)
  }
  final class VarsBuilder[A] private[PropagationLang](n: Int) {
    def apply[D: Domain[A, ?] : BoundedLattice](): FP[Vector[DomRef[A, D]]] = apply[D](BoundedLattice[D].one)
    def apply[D: Domain[A, ?]](d: D): FP[Vector[DomRef[A, D]]] =
      Traverse[Vector].sequenceU(Vector.fill(n)(variable(d)))

    def oneOf(s: Set[A]): FP[Vector[DomRef[A, Set[A]]]] = apply(s)
  }

  // constructors returning less specific types, and curried to help with type inference
  def intersect[K[_], D](ref: CellRef[D])(d: D): PropagationLang[K, Unit] = Intersect(ref, d)
  def intersectVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N])(values: Sized[Vector[D], N]): PropagationLang[K, Unit] = IntersectVector(refs, values)
  def fetch[K[_], D](ref: CellRef[D]): PropagationLang[K, D] = Fetch(ref)
  def fetchVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): PropagationLang[K, Sized[Vector[D], N]] = FetchVector(refs)
  def varTrigger[K[_], D](ref: CellRef[D])(f: D => Trigger[K]): PropagationLang[K, Unit] = VarTrigger(ref, f)
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K]): PropagationLang[K, Unit] = SelTrigger(sel, f)
  def whenResolved[K[_], A, D](ref: DomRef[A, D])(f: A => K[Unit]): PropagationLang[K, Unit] = WhenResolved(ref, f)

  // constructors lifted to free programs
  def intersectF[D](ref: CellRef[D])(d: D): FP[Unit] =
    lift(intersect[FP, D](ref)(d))
  def intersectVectorF[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N])(values: Sized[Vector[D], N]): FP[Unit] =
    lift(intersectVector[FP, D, N](refs)(values))
  def fetchF[D](ref: CellRef[D]): FP[D] =
    lift(fetch[FP, D](ref))
  def fetchVectorF[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): FP[Sized[Vector[D], N]] =
    lift(fetchVector[FP, D, N](refs))
  def varTriggerF[F[_[_], _], D](ref: CellRef[D])(f: D => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    liftF(varTrigger[FreeK[F, ?], D](ref)(f))
  def selTriggerF[F[_[_], _], L <: HList](sel: Sel[L])(f: L => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    liftF(selTrigger[FreeK[F, ?], L](sel)(f))
  def whenResolvedF[F[_[_], _], A, D](ref: DomRef[A, D])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    liftF(whenResolved[FreeK[F, ?], A, D](ref)(f))


  // convenience API
  def set[A, D: Domain[A, ?]](ref: DomRef[A, D], a: A): FP[Unit] =
    intersectF(ref)(Domain[A, D].singleton(a))
  def remove[A, D: Domain[A, ?] : GenBool](ref: DomRef[A, D], a: A): FP[Unit] = {
    val d = Domain[A, D].singleton(a)
    fetchF(ref) >>= { d0 => intersectF(ref)(GenBool[D].without(d0, d)) }
  }
  def selTrigger2[K[_], D1, D2](ref1: CellRef[D1], ref2: CellRef[D2])(f: (D1, D2) => Trigger[K]): PropagationLang[K, Unit] =
    selTrigger[K, D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))
  def selTrigger2F[F[_[_], _], D1, D2](ref1: CellRef[D1], ref2: CellRef[D2])(f: (D1, D2) => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    liftF(selTrigger2[FreeK[F, ?], D1, D2](ref1, ref2)(f))

  private def lift[A](p: PropagationLang[FreeK[PropagationLang, ?], A]): FP[A] =
    FreeK.lift(p)

  private def liftF[F[_[_], _], A](p: PropagationLang[FreeK[F, ?], A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, A] =
    FreeK.lift(inj.inj[FreeK[F, ?], A](p))

  implicit def functorKInstance: FunctorKA[PropagationLang] = new FunctorKA[PropagationLang] {

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: ~>[K, L]): PropagationLang[L, A] = pk match {

      // the interesting cases
      case VarTrigger(ref, f)   => varTrigger(ref){ d => FunctorK[Trigger].transform(f(d))(tr) }
      case SelTrigger(sel, f)   => selTrigger(sel){ l => FunctorK[Trigger].transform(f(l))(tr) }
      case WhenResolved(ref, f) => whenResolved(ref){ x => tr(f(x)) }

      // the boring cases
      case Variable(d, dom)            => Variable(d, dom)
      case Intersect(ref, d)           => Intersect(ref, d)
      case IntersectVector(refs, vals) => IntersectVector(refs, vals)
      case Fetch(ref)                  => Fetch(ref)
      case FetchVector(refs)           => FetchVector(refs)
    }
  }
}