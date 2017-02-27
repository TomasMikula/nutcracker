package nutcracker

import nutcracker.util.ops.applicative._
import scalaz.Applicative

trait BranchingPropagation[M[_], Ref[_]] {
  implicit def propagation: Propagation[M, Ref]

  def newVar[A](a: A)(implicit ev: Splittable[A]): M[Ref[A]]

  def newVar[A](implicit ev: SplittableDomWithBottom[A]): M[Ref[A]] =
    newVar(ev.bottom)

  def vars[D](d: D, n: Int)(implicit dom: Splittable[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    newVar(d).replicate(n)
}

object BranchingPropagation {

  def module[Ref0[_]]: PersistentBranchingModule { type Ref[A] = Ref0[A] } =
    new BranchingModuleImpl[Ref0]

  implicit def toPropagation[M[_], Ref[_]](implicit bp: BranchingPropagation[M, Ref]): Propagation[M, Ref] =
    bp.propagation
}