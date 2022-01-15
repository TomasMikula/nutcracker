package nutcracker

import nutcracker.util.ops.applicative._
import scalaz.Monad

trait BranchingPropagation[M[_]] {
  type Propagation <: nutcracker.Propagation[M]

  implicit val propagation: Propagation

  export propagation.{Var, Val}

  implicit def M: Monad[M] = propagation.M

  def newVar[A](a: A)(implicit ev: Splittable[A]): M[Var[A]]

  def newVar[A](implicit ev: SplittableDomWithBottom[A]): M[Var[A]] =
    newVar(ev.bottom)

  def vars[D](d: D, n: Int)(implicit dom: Splittable[D]): M[Vector[Var[D]]] =
    newVar(d).replicate(n)
}

object BranchingPropagation {
  type Aux0[M[_], Var0[_]] = BranchingPropagation[M] {
    type Propagation <: nutcracker.Propagation.Aux0[M, Var0]
  }

  type Aux1[M[_], Var0[_], Val0[_]] = BranchingPropagation[M] {
    type Propagation <: nutcracker.Propagation.Aux1[M, Var0, Val0]
  }

  implicit def toPropagation[M[_]](implicit bp: BranchingPropagation[M]): bp.propagation.type =
    bp.propagation
}