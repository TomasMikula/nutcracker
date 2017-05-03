package nutcracker.data

import nutcracker.BranchingPropagation._
import nutcracker.ops._
import nutcracker.{BranchingPropagation, Final, RelativelyComplementedDom, Splittable, SplittableJoinDom, TerminalDom, UpdateResult}
import scalaz.syntax.bind._
import scalaz.{Bind, Cont}

/** Decreasing set.
  * A wrapper for `Set` where a _monotonic_ update is one that removes
  * elements, e.g. by intersection or set difference. This is dual to
  * the usual view of the poset of sets: here, smaller sets (by subset
  * relation) are considered greater (i.e. are higher in the lattice),
  * set intersection is the join and set union is the meet.
  */
final class DecSet[A] private(private val value: Set[A]) extends AnyVal {
  def size: Int = value.size
  def contains(a: A): Boolean = value.contains(a)
  def head: A = value.head
  def tail: DecSet[A] = new DecSet(value.tail)
  def intersect(that: DecSet[A]): DecSet[A] = new DecSet(this.value intersect that.value)
  def diff(that: DecSet[A]): DecSet[A] = new DecSet(this.value diff that.value)
  def toList: List[A] = value.toList
}

object DecSet {
  sealed abstract class Update[A]
  final case class Join[A](value: DecSet[A]) extends Update[A]
  final case class Diff[A](value: DecSet[A]) extends Update[A]

  final case class Removed[A](value: Set[A]) extends AnyVal
  final type Delta[A] = Removed[A]

  type Dom[A] = SplittableJoinDom.Aux[DecSet[A], Update[A], Delta[A]] with RelativelyComplementedDom[DecSet[A]] with TerminalDom[DecSet[A]]

  def apply[A](as: A*): DecSet[A] = new DecSet(Set(as: _*))
  def singleton[A](a: A): DecSet[A] = new DecSet(Set(a))
  def wrap[A](as: Set[A]): DecSet[A] = new DecSet(as)

  def init[M[_], Var[_], Val[_], A](as: Set[A])(implicit M: BranchingPropagation[M, Var, Val]): M[Var[DecSet[A]]] =
    M.newVar(wrap(as))
  def oneOf[M[_], Var[_], Val[_], A](as: A*)(implicit M: BranchingPropagation[M, Var, Val]): M[Var[DecSet[A]]] =
    init(as.toSet)

  /** Convenience method to add an exclusive choice of arbitrary free programs
    * to continue. When the choice is made, the chosen program is executed.
    */
  def branchAndExec[M[_], Var[_], Val[_]](conts: Set[M[Unit]])(implicit M: BranchingPropagation[M, Var, Val], B: Bind[M]): M[Unit] =
    init(conts) >>= { _.whenFinal_(k => k) }
  def branchAndExec[M[_], Var[_], Val[_]](conts: M[Unit]*)(implicit M: BranchingPropagation[M, Var, Val], B: Bind[M]): M[Unit] =
    branchAndExec(conts.toSet)

  /** Convenience method to add an exclusive choice of multiple possibilities,
    * presented as a continuation of the chosen element. Note that a "branching
    * cell" (see [[init]]) is added for each callback that
    * is registered on the returned continuation. Thus, if two callbacks are
    * registered on the returned continuation, it will have the effect of making
    * a choice from the cartesian product `as × as`. If this is not desired,
    * use [[init]] directly.
    */
  def branchC[M[_], Var[_], Val[_], A](as: Set[A])(implicit M: BranchingPropagation[M, Var, Val], B: Bind[M]): Cont[M[Unit], A] =
    Cont(f => init(as) >>= { _.asCont_.run(f) })
  def branchC[M[_], Var[_], Val[_], A](as: A*)(implicit M: BranchingPropagation[M, Var, Val], B: Bind[M]): Cont[M[Unit], A] =
    branchC(as.toSet)

  implicit def domInstance[A]: DecSet.Dom[A] =
    new SplittableJoinDom[DecSet[A]] with RelativelyComplementedDom[DecSet[A]] with TerminalDom[DecSet[A]] {
      type Update = DecSet.Update[A]
      type Delta = DecSet.Delta[A]

      import Splittable._
      override def assess(d: DecSet[A]): Status[Update] = d.size match {
        case 0 => Failed
        case 1 => Refined
        case _ => Unrefined(() => Some(d.toList map (x => Join(singleton(x))))) // split into singleton sets
      }

      override def update[S <: DecSet[A]](s: S, u: Update): UpdateResult[DecSet[A], IDelta, S] = u match {
        case Join(m) => intersect(s, m)
        case Diff(d) => diff(s, d)
      }

      def toJoinUpdate(d: DecSet[A]) = Join(d)
      def toComplementUpdate(d: DecSet[A]) = Diff(d)

      override def appendDeltas(d1: Delta, d2: Delta): Delta =
        Removed(d1.value union d2.value)

      override def terminate: Update = Join(DecSet())

      @inline
      private def intersect[D <: DecSet[A]](a: DecSet[A], b: DecSet[A]): UpdateResult[DecSet[A], IDelta, D] = {
        val res = a intersect b
        if(res.size < a.size) UpdateResult(res, Removed(a.value diff b.value))
        else UpdateResult()
      }

      @inline
      private def diff[D <: DecSet[A]](a: DecSet[A], b: DecSet[A]): UpdateResult[DecSet[A], IDelta, D] = {
        val res = a diff b
        if(res.size < a.size) UpdateResult(res, Removed(a.value intersect b.value))
        else UpdateResult()
      }
    }

  implicit def finalInstance[A]: Final.Aux[DecSet[A], A] = new Final[DecSet[A]] {
    type Out = A

    def extract(d: DecSet[A]): Option[A] = if(d.size == 1) Some(d.head) else None

    def embed(a: A): DecSet[A] = singleton(a)
  }

}