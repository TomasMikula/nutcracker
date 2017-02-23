package nutcracker

import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import nutcracker.util.ops.applicative._
import scalaz.{Applicative, ~>}
import scalaz.Id._

trait BranchingPropagation[M[_], Ref[_]] {
  implicit def propagation: Propagation[M, Ref]

  def newVar[A](a: A)(implicit ev: Splittable[A]): M[Ref[A]]

  def newVar[A](implicit ev: SplittableDomWithBottom[A]): M[Ref[A]] =
    newVar(ev.bottom)

  def vars[D](d: D, n: Int)(implicit dom: Splittable[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    newVar(d).replicate(n)
}

object BranchingPropagation {

  trait Module[P <: Propagation.Module] {
    val Prop: P

    type Ref[A] = Prop.Ref[A]
    type Lang[K[_], A]
    type State[K[_]]

    implicit def freeBranchingPropagation[F[_[_], _]](implicit
      i: InjectK[Lang, F],
      j: InjectK[Prop.Lang, F]
    ): BranchingPropagation[FreeK[F, ?], Ref]

    def empty[K[_]]: State[K]
    def interpreter: Step[Lang, State]
    def assess[K[_]](s: State[K])(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]]
  }

  def module(prop: Propagation.Module): Module[prop.type] =
    new BranchingPropagationModuleImpl(prop)
}

private[nutcracker] class BranchingPropagationModuleImpl[P <: Propagation.Module](val Prop: P) extends BranchingPropagation.Module[P] {
  type Lang[K[_], A] = BranchLang[Ref, K, A]
  type State[K[_]] = BranchStore[Ref, K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    j: InjectK[Prop.Lang, F]
  ): BranchingPropagation[FreeK[F, ?], Ref] =
    new BranchingPropagation[FreeK[F, ?], Ref] {
      val propagation: Propagation[FreeK[F, ?], Ref] = Prop.freePropagation[F]

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, Ref[A]] =
        if(ev.isUnresolved(a))
          for {
            ref <- propagation.newCell[A](a)
            _ <- BranchLang.trackF(ref)
            _ <- propagation.observe(ref).threshold(a =>
              if(ev.isUnresolved(a)) None
              else Some(BranchLang.untrackF(ref))
            )
          } yield ref
        else
          propagation.newCell[A](a)
    }

  def empty[K[_]]: State[K] = BranchStore()

  def interpreter: Step[Lang, State] = new Step[Lang, State] {
    import BranchLang._
    def apply[K[_], A](f: BranchLang[Ref, K, A]): WriterState[Lst[K[Unit]], State[K], A] = f match {
      case Track(ref, ev) => WriterState(s => (Lst.empty, s.addVar(ref, ev), ()))
      case Untrack(ref) => WriterState(s => (Lst.empty, s.removeVar(ref), ()))
    }
  }

  def assess[K[_]](s: State[K])(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]] =
    s.split(fetch)
}