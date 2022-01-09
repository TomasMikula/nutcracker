package nutcracker

import nutcracker.util.ops.applicative._
import scalaz.Applicative

trait BranchingPropagation[M[_], Var[_], Val[_]] {
  implicit def propagation: Propagation.Aux[M, Var, Val]

  def newVar[A](a: A)(implicit ev: Splittable[A]): M[Var[A]]

  def newVar[A](implicit ev: SplittableDomWithBottom[A]): M[Var[A]] =
    newVar(ev.bottom)

  def vars[D](d: D, n: Int)(implicit dom: Splittable[D], M: Applicative[M]): M[Vector[Var[D]]] =
    newVar(d).replicate(n)
}

object BranchingPropagation {

  implicit def toPropagation[M[_], Var[_], Val[_]](implicit bp: BranchingPropagation[M, Var, Val]): Propagation.Aux[M, Var, Val] =
    bp.propagation
}