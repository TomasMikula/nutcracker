package nutcracker

import scala.language.existentials

import algebra.lattice.MeetSemilattice
import shapeless.Coproduct
import shapeless.Generic
import shapeless.HList
import shapeless.Nat
import shapeless.Sized
import shapeless.ops.coproduct.ToHList
import shapeless.ops.hlist.Mapped
import nutcracker.algebraic.BoolRng
import nutcracker.algebraic.BoolRing

import scalaz.{Traverse, Monad}
import scalaz.std.vector._

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
  private[nutcracker] case class Variable[A, D](d: D, dom: Domain[A, D]) extends ProblemDescription[PureDomRef[A, D]]
  private[nutcracker] case class VarTrigger[A, D](ref: PureDomRef[A, D], f: D => ProblemDescription[Unit]) extends ProblemDescription[Unit]
  private[nutcracker] case class Intersect[A, D](ref: PureDomRef[A, D], d: D) extends ProblemDescription[Unit]
  private[nutcracker] case class IntersectVector[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N], values: Sized[Vector[D], N]) extends ProblemDescription[Unit]
  private[nutcracker] case class Fetch[A, D](ref: PureDomRef[A, D]) extends ProblemDescription[D]
  private[nutcracker] case class FetchVector[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N]) extends ProblemDescription[Sized[Vector[D], N]]
  private[nutcracker] case class WhenResolved[A, D, B](ref: PureDomRef[A, D], f: A => ProblemDescription[B]) extends ProblemDescription[B]


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


  class VariableStub[A] {
    def apply[D: Domain[A, ?] : BoolRing](): ProblemDescription[PureDomRef[A, D]] = apply(BoolRing[D].top)
    def apply[D: Domain[A, ?] : BoolRng](d: D): ProblemDescription[PureDomRef[A, D]] = Variable(d, implicitly[Domain[A, D]])

//    def composeOf[PA <: HList, PDA <: HList, PDS <: PDA](components: PDS)(implicit
//        pa: Generic.Aux[A, PA],
//        pda: Mapped.Aux[PA, λ[X => ProblemDescription[DomRef[X]]], PDA]) = ???

//    def branchByCase[CA <: Coproduct, PA <: HList, Cases <: HList](cases: Cases)(implicit
//      ca: Generic.Aux[A, CA],
//      pa: ToHList.Aux[CA, PA],
//      m: Mapped.Aux[PA, λ[X => () => ProblemDescription[DomRef[X]]], Cases]): ProblemDescription[DomRef[A]] = CoproductVariable(cases)
  }

//  class ComposeStub[A] {
//    def by[L <: HList](f: L => A): ComposeByStub[A, L] = new ComposeByStub[A, L](f)
//    def from[PA <: HList, RA <: HList, Refs <: RA](refs: Refs)(implicit
//      pa: Generic.Aux[A, PA],
//      ra: Mapped.Aux[PA, DomRef, RA]): ProblemDescription[DomRef[A]] = ??? //ProductVariable[A, PA, RA, Refs](refs)
//  }

//  class ComposeByStub[A, L <: HList](f: L => A) {
//    def from(args: Mapped[L, DomRef]#Out): ProblemDescription[DomRef[A]] = ???
//  }

  def pure[A](a: A): ProblemDescription[A] = Pure(a)
  def variable[A]: VariableStub[A] = new VariableStub[A]
  def varTrigger[A, D](ref: PureDomRef[A, D])(f: D => ProblemDescription[Unit]): ProblemDescription[Unit] =
    VarTrigger(ref, f)
  def partialVarTrigger[A, D](ref: PureDomRef[A, D])(f: PartialFunction[D, ProblemDescription[Unit]]): ProblemDescription[Unit] =
    VarTrigger(ref, (d: D) => if(f.isDefinedAt(d)) f(d) else Pure(()))
//  def compose[A]: ComposeStub[A] = new ComposeStub[A]
  def fetch[D](ref: PureDomRef[_, D]): ProblemDescription[D] = Fetch(ref)
  def fetchVector[D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N] forSome { type A }): ProblemDescription[Sized[Vector[D], N]] = FetchVector(refs)
  def fetchResult[A, D](ref: PureDomRef[A, D]): ProblemDescription[A] = WhenResolved[A, D, A](ref, a => Pure(a))
  def fetchResults[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N]): ProblemDescription[Sized[Vector[A], N]] = {
    Traverse[Vector].traverse(refs.unsized)(ref => fetchResult(ref)) map { Sized.wrap(_) }
  }
  def branch2[A, B](a: () => ProblemDescription[A], b: () => ProblemDescription[B]): ProblemDescription[Either[A, B]] =
    branch[Either[A, B]](() => a().map(Left(_)), () => b().map(Right(_)))
  def branch[A](branches: () => List[ProblemDescription[A]]): ProblemDescription[A] = Branch(branches)
  def branch[A](branches: (() => ProblemDescription[A])*): ProblemDescription[A] = Branch(() => branches.map(_.apply()).toList)
  def empty[A]: ProblemDescription[A] = branch(() => Nil)
  def whenComplete[A, B](pr: PromiseId[A])(f: A => ProblemDescription[B]): ProblemDescription[B] = WhenComplete(pr, f)
  def intersect[D](ref: PureDomRef[_, D], a: D): ProblemDescription[Unit] = Intersect(ref, a)
  def intersectVector[D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N] forSome { type A }, values: Sized[Vector[D], N]): ProblemDescription[Unit] = IntersectVector(refs, values)
//  def constraint2[A, B](c: Constraint[(A, B)], domA: PureDomRef[_, A], domB: PureDomRef[_, B]): ProblemDescription[Unit] = ???
  def vectorConstraint[D: MeetSemilattice, N <: Nat](
      c: Constraint[Sized[Vector[D], N]],
      refs: Sized[Vector[PureDomRef[A, D]], N] forSome { type A }): ProblemDescription[Unit] = {
    val trigger: D => ProblemDescription[Unit] =
      d => fetchVector(refs).flatMap(doms => intersectVector(refs, c.enforce(doms)))
    Traverse[Vector].traverse(refs.unsized)(ref => VarTrigger(ref, trigger)) map { vectorOfUnit => () }
    // TODO relation based constraints
  }
}