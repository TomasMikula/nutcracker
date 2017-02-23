package nutcracker

import nutcracker.ops._
import scala.language.higherKinds
import scalaz.{Bind, Cont}
import scalaz.syntax.bind._

/** API for branching as a special kind of lattice. */
class Branching[M[_], Ref[_]](implicit M: BranchingPropagation[M, Ref]) {
  implicit val prop = M.propagation

  /** Convenience method to add an exclusive choice of multiple possibilities.
    * This is a shorthand for adding a cell whose semi-lattice is the lattice
    * of finite sets of elements of type A, initialized to the given set of
    * elements.
    */
  def branch[A](as: Set[A]): M[Ref[DecSet[A]]] = DecSet.oneOf(as)
  def branch[A](as: A*): M[Ref[DecSet[A]]] = branch(as.toSet)

  /** Convenience method to add an exclusive choice of arbitrary free programs
    * to continue. When the choice is made, the chosen program is executed.
    */
  def branchAndExec(conts: Set[M[Unit]])(implicit B: Bind[M]): M[Unit] =
    branch(conts) >>= { _.whenFinal(k => k) }
  def branchAndExec(conts: M[Unit]*)(implicit B: Bind[M]): M[Unit] =
    branchAndExec(conts.toSet)

  /** Convenience method to add an exclusive choice of multiple possibilities,
    * presented as a continuation of the chosen element. Note that a "branching
    * cell" (see [[branch(Set[A])]]) is added for each callback that is registered
    * on the returned continuation. Thus, if two callback are registered on the
    * returned continuation, it will have the effect of making a choice from the
    * cartesian product `as × as`. If this is not what you want, use
    * [[branch(Set[A])]] directly.
    */
  def branchC[A](as: Set[A])(implicit B: Bind[M]): Cont[M[Unit], A] =
    Cont(f => branch(as) >>= { _.asCont.apply(f) })
  def branchC[A](as: A*)(implicit B: Bind[M]): Cont[M[Unit], A] =
    branchC(as.toSet)
}

object Branching {
  def apply[M[_], Ref[_]](implicit P: BranchingPropagation[M, Ref]): Branching[M, Ref] = new Branching[M, Ref]
}