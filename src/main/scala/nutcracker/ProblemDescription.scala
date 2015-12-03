package nutcracker

import scala.language.existentials

import algebra.lattice.{GenBool, BoundedLattice, MeetSemilattice}
import scalaz.{Traverse, Monad}
import scalaz.std.vector._
import shapeless._
import shapeless.ops.hlist.Mapped

sealed trait ProblemDescription[+A] {
  import ProblemDescription._

  def flatMap[B](f: A => ProblemDescription[B]): ProblemDescription[B] = Bind(this, f)
  def >>=[B](f: A => ProblemDescription[B]): ProblemDescription[B] = Bind(this, f)
  def map[B](f: A => B): ProblemDescription[B] = Bind(this, (a: A) => Pure(f(a)))
  def zipWith[B](b: ProblemDescription[B]): ProblemDescription[(A, B)] = Zip(this, b)
}

object ProblemDescription {

  implicit class ProblemDescriptionUnitOps(val pd: ProblemDescription[Unit]) extends AnyVal {
    def >>[B](b: ProblemDescription[B]): ProblemDescription[B] = Second(pd, b)
  }

  implicit val problemDescriptionMonad: Monad[ProblemDescription] = new Monad[ProblemDescription] {
    override def bind[A, B](fa: ProblemDescription[A])(f: (A) => ProblemDescription[B]): ProblemDescription[B] = fa.flatMap(f)
    override def point[A](a: => A): ProblemDescription[A] = Pure(a)
  }


  /* general combinatory operators */

  private[nutcracker] case class Pure[A](value: A) extends ProblemDescription[A]
  private[nutcracker] case class Second[A](p1: ProblemDescription[Unit], p2: ProblemDescription[A]) extends ProblemDescription[A]
  private[nutcracker] case class Zip[A, B](a: ProblemDescription[A], b: ProblemDescription[B]) extends ProblemDescription[(A, B)] {
    type Fst = A
    type Snd = B
  }
  private[nutcracker] case class Bind[A, B](desc: ProblemDescription[A], f: A => ProblemDescription[B]) extends ProblemDescription[B]


  /* domain specific instructions */

  // working with variables
  private[nutcracker] case class Variable[A, D](d: D, dom: Domain[A, D]) extends ProblemDescription[DomRef[A, D]]
  private[nutcracker] case class VarTrigger[D](ref: CellRef[D], f: D => Trigger[ProblemDescription]) extends ProblemDescription[Unit]
  private[nutcracker] case class SelTrigger[L <: HList](sel: Sel[L], f: L => Trigger[ProblemDescription]) extends ProblemDescription[Unit]
  private[nutcracker] case class Intersect[D](ref: CellRef[D], d: D) extends ProblemDescription[Unit]
  private[nutcracker] case class IntersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]) extends ProblemDescription[Unit]
  private[nutcracker] case class Fetch[A, D](ref: DomRef[A, D]) extends ProblemDescription[D]
  private[nutcracker] case class FetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]) extends ProblemDescription[Sized[Vector[D], N]]
  private[nutcracker] case class FetchResult[A, D](ref: DomRef[A, D]) extends ProblemDescription[A]
  private[nutcracker] case class WhenResolved[A, D](ref: DomRef[A, D], f: A => ProblemDescription[Unit]) extends ProblemDescription[Unit]


  // working with relations

  private[nutcracker] case class Relation[L <: HList, Refs <: HList](
      rel: Rel[L],
      refs: Refs,
      toRefs: Mapped.Aux[L, CellRef, Refs]) extends ProblemDescription[Unit]

  private[nutcracker] case class RelTrigger[L <: HList, Refs <: HList](
      rel: Rel[L],
      f: Refs => ProblemDescription[Unit],
      toRefs: Mapped.Aux[L, CellRef, Refs]) extends ProblemDescription[Unit]


  // branching
  private[nutcracker] case class Branch[+A](branches: () => List[ProblemDescription[A]]) extends ProblemDescription[A] {
    def flatMap1[B](f: A => ProblemDescription[B]): Branch[B] = Branch(() => branches() map { _.flatMap(f) })
    def map1[B](f: A => B): Branch[B] = Branch(() => branches() map { _.map(f) })
  }

  // working with promises
  private[nutcracker] case class Complete[A](pr: PromiseId[A], a: A) extends ProblemDescription[Unit]
  private[nutcracker] case class WhenComplete[A, B](pr: PromiseId[A], f: A => ProblemDescription[B]) extends ProblemDescription[B]


  final class VariableStub[A] private[nutcracker] {
    def apply[D: Domain[A, ?] : BoundedLattice](): ProblemDescription[DomRef[A, D]] = domain()
    def apply[D: Domain[A, ?]](d: D): ProblemDescription[DomRef[A, D]] = domain(d)

    def domain[D: Domain[A, ?] : BoundedLattice](): ProblemDescription[DomRef[A, D]] = domain(BoundedLattice[D].one)
    def domain[D: Domain[A, ?]](d: D): ProblemDescription[DomRef[A, D]] = Variable(d, implicitly[Domain[A, D]])

    def oneOf(s: Set[A]): ProblemDescription[DomRef[A, Set[A]]] = Variable(s, implicitly[Domain[A, Set[A]]])
  }

  final class VariablesStub[A] private[nutcracker](n: Int) {
    def apply[D: Domain[A, ?] : BoundedLattice](): ProblemDescription[Vector[DomRef[A, D]]] = apply[D](BoundedLattice[D].one)
    def apply[D: Domain[A, ?] : BoundedLattice](d: D): ProblemDescription[Vector[DomRef[A, D]]] =
      Traverse[Vector].sequenceU(Vector.fill(n)(variable(d)))

    def oneOf(s: Set[A]): ProblemDescription[Vector[DomRef[A, Set[A]]]] =
      Traverse[Vector].sequenceU(Vector.fill(n)(variable(s)))
  }

  def pure[A](a: A): ProblemDescription[A] = Pure(a)
  def variable[A]: VariableStub[A] = new VariableStub[A]
  def variables[A](n: Int): VariablesStub[A] = new VariablesStub[A](n)
  def varTrigger[D](ref: CellRef[D])(f: D => Trigger[ProblemDescription]): ProblemDescription[Unit] =
    VarTrigger(ref, f)
  def partialVarTrigger[D](ref: CellRef[D])(f: PartialFunction[D, Trigger[ProblemDescription]]): ProblemDescription[Unit] =
    VarTrigger(ref, (d: D) => if(f.isDefinedAt(d)) f(d) else Sleep())
  def selectionTrigger[L <: HList](sel: Sel[L])(f: L => Trigger[ProblemDescription]): ProblemDescription[Unit] = SelTrigger(sel, f)
  def selectionTrigger2[D1, D2](ref1: CellRef[D1], ref2: CellRef[D2])(f: (D1, D2) => Trigger[ProblemDescription]): ProblemDescription[Unit] =
    selectionTrigger[D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))
  def fetch[D](ref: DomRef[_, D]): ProblemDescription[D] = Fetch(ref)
  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): ProblemDescription[Sized[Vector[D], N]] = FetchVector(refs)
  def fetchResult[A, D](ref: DomRef[A, D]): ProblemDescription[A] = FetchResult[A, D](ref)
  def fetchResults[A, D](refs: Vector[DomRef[A, D]]): ProblemDescription[Vector[A]] =
    Traverse[Vector].traverse(refs){ fetchResult(_) }
  def fetchResults[A, D](refs: DomRef[A, D]*): ProblemDescription[Seq[A]] = fetchResults(refs.toVector)
  def whenResolved[A, D](ref: DomRef[A, D])(f: A => ProblemDescription[Unit]): ProblemDescription[Unit] = WhenResolved(ref, f)
  def branch2[A, B](a: () => ProblemDescription[A], b: () => ProblemDescription[B]): ProblemDescription[Either[A, B]] =
    branch[Either[A, B]](() => a().map(Left(_)), () => b().map(Right(_)))
  def branch[A](branches: () => List[ProblemDescription[A]]): ProblemDescription[A] = Branch(branches)
  def branch[A](branches: (() => ProblemDescription[A])*): ProblemDescription[A] = Branch(() => branches.map(_.apply()).toList)
  def empty[A]: ProblemDescription[A] = branch(() => Nil)
  def whenComplete[A, B](pr: PromiseId[A])(f: A => ProblemDescription[B]): ProblemDescription[B] = WhenComplete(pr, f)
  def intersect[D](ref: CellRef[D], a: D): ProblemDescription[Unit] = Intersect(ref, a)
  def set[A, D](ref: DomRef[A, D], a: A)(implicit dom: Domain[A, D]): ProblemDescription[Unit] = Intersect(ref, dom.singleton(a))
  def intersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]): ProblemDescription[Unit] = IntersectVector(refs, values)
  def concat(pds: Iterable[ProblemDescription[Unit]]): ProblemDescription[Unit] =
    pds.foldLeft[ProblemDescription[Unit]](Pure(())) { _ >> _ }
  def remove[A, D: Domain[A, ?] : GenBool](ref: DomRef[A, D])(a: A): ProblemDescription[Unit] = {
    val d = Domain[A, D].singleton(a)
    fetch(ref) >>= { d0 => intersect(ref, GenBool[D].without(d0, d)) }
  }

  def vectorConstraint[D: MeetSemilattice, N <: Nat](
      c: Constraint[Sized[Vector[D], N]],
      refs: Sized[Vector[CellRef[D]], N]): ProblemDescription[Unit] = {
    val trigger: D => Trigger[ProblemDescription] =
      d => FireReload(fetchVector(refs).flatMap(doms => intersectVector(refs, c.enforce(doms))))
    Traverse[Vector].traverse(refs.unsized)(ref => varTrigger(ref)(trigger)) map { vectorOfUnit => () }
    // TODO relation based constraints
  }
}