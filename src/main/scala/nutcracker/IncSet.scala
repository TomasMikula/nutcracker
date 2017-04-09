package nutcracker

import nutcracker.ops._
import nutcracker.util.{ContU, DeepEqual, DeepShow, IsEqual, MonadObjectOutput}
import scalaz.{Applicative, Bind, Functor, IndexedContT, Monad}
import scalaz.std.list._
import scalaz.syntax.bind._

/** Increasing set.
  * A wrapper for `Set` where a _monotonic_ update is one that adds
  * elements, e.g. by union with another set.
  */
final class IncSet[A] private(val value: Set[A]) extends AnyVal {
  def size: Int = value.size
  def contains(a: A): Boolean = value.contains(a)
  def head: A = value.head
  def union(that: IncSet[A]): IncSet[A] = new IncSet(this.value union that.value)
  def toList: List[A] = value.toList
}

object IncSet {
  def apply[A](as: A*): IncSet[A] = new IncSet(Set(as: _*))
  def empty[A]: IncSet[A] = new IncSet(Set())
  def singleton[A](a: A): IncSet[A] = new IncSet(Set(a))
  def wrap[A](as: Set[A]): IncSet[A] = new IncSet(as)

  final case class Join[A](value: IncSet[A]) // extends AnyVal // value class may not wrap another user-defined value class
  final case class Added[A](value: Set[A]) extends AnyVal

  type Update[A] = Join[A]
  type Delta[A] = Added[A]

  type IncSetDom[A] = Dom.Aux[IncSet[A], Update[A], Delta[A]]

  implicit def domInstance[A]: IncSetDom[A] = new Dom[IncSet[A]] {
    type Update = IncSet.Update[A]
    type Delta = IncSet.Delta[A]

    override def isFailed(d: IncSet[A]): Boolean = false

    override def update[S <: IncSet[A]](s: S, u: Update): UpdateResult[IncSet[A], IDelta, S] = {
      val res = s union u.value
      if(res.size > s.size) UpdateResult(res, Added(u.value.value diff s.value))
      else UpdateResult()
    }

    override def appendDeltas(d1: Delta, d2: Delta): Delta =
      Added(d1.value union d2.value)
  }

  implicit def deepEqual[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[IncSet[A1], IncSet[A2], Ptr1, Ptr2] =
    new DeepEqual[IncSet[A1], IncSet[A2], Ptr1, Ptr2] {
      def equal(s1: IncSet[A1], s2: IncSet[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.setEqual(s1.value, s2.value)
    }

  implicit def deepShow[Ptr[_], A](implicit ev: DeepShow[A, Ptr]): DeepShow[IncSet[A], Ptr] =
    new DeepShow.FromSerialize[IncSet[A], Ptr] {
      def serialize[M[_]](a: IncSet[A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
        DeepShow.set(ev).serialize(a.value)
    }
}


class IncSets[F[_], Var[_], Val[_]](implicit P: Propagation[F, Var, Val]) {
  import IncSet._

  def init[A]: F[Var[IncSet[A]]] =
    P.newCell(IncSet.empty[A])

  /** Returns the given set in a CPS style, executing any subsequently
    * given callback for every current and future element of that set.
    */
  def forEach[A](ref: Var[IncSet[A]])(implicit F: Applicative[F]): IndexedContT[F, Subscription[F], Unit, A] = {
    import scalaz.syntax.traverse._
    IndexedContT(f => ref.asVal.observe.by(as => {
      val now = as.toList.traverse_(f)
      val onChange = Trigger.continually((as: IncSet[A], delta: Delta[A]) => delta.value.toList.traverse_(f))
      Trigger.fireReload(now.as(Trigger.sleep(onChange)))
    }))
  }

  def forEach_[A](ref: Var[IncSet[A]])(implicit F: Applicative[F]): ContU[F, A] = {
    val cps = forEach(ref)
    ContU(f => cps(f).void)
  }

  def insert[A](a: A, into: Var[IncSet[A]]): F[Unit] =
    insertAll(Set(a), into)

  def insertAll[A](add: Set[A], into: Var[IncSet[A]]): F[Unit] =
    P.update(into).by(Join(IncSet.wrap(add)))

  def include[A](sub: Var[IncSet[A]], sup: Var[IncSet[A]])(implicit F: Functor[F]): F[Unit] =
    sub.asVal.observe.by((sa: IncSet[A]) => {
      val now = insertAll(sa.value, sup)
      val onChange = Trigger.continually((sa: IncSet[A], delta: Delta[A]) => insertAll(delta.value, sup))
      Trigger.fireReload(now.as(Trigger.sleep(onChange)))
    }).void

  def includeC[A](cps: ContU[F, A], ref: Var[IncSet[A]]): F[Unit] =
    cps(a => insert(a, ref))

  def collect[A](cps: ContU[F, A])(implicit B: Bind[F]): F[Var[IncSet[A]]] = for {
    res <- init[A]
    _   <- includeC(cps, res)
  } yield res

  def collectAll[A](cps: ContU[F, A]*)(implicit M: Monad[F]): F[Var[IncSet[A]]] =
    collectAll(cps)

  def collectAll[A](cps: Iterable[ContU[F, A]])(implicit M: Monad[F]): F[Var[IncSet[A]]] =
    collect(ContU.sequence(cps))

  /** Relative monadic bind. `Var[IncSet[A]]` is a monad relative to `F`,
    * i.e. we can implement `bind` if additional effects of type `F` are allowed.
    * This is equivalent to having a monad instance for `λ[A => F[Var[IncSet[A]]]]`.
    */
  def relBind[A, B](sref: Var[IncSet[A]])(f: A => F[Var[IncSet[B]]])(implicit M: Monad[F]): F[Var[IncSet[B]]] = {
    import scalaz.syntax.traverse._
    for {
      res <- init[B]
      _ <- sref.asVal.observe.by((sa: IncSet[A]) => {
        val now = sa.toList.traverse_(f(_) >>= (refb => include(refb, res)))
        val onChange = Trigger.continually((sa: IncSet[A], delta: Delta[A]) => delta.value.toList.traverse_(f(_) >>= (refb => include(refb, res))))
        Trigger.fireReload(now.as(Trigger.sleep(onChange)))
      })
    } yield res
  }

  implicit def monad(implicit M: Monad[F]): Monad[λ[A => F[Var[IncSet[A]]]]] =
    new Monad[λ[A => F[Var[IncSet[A]]]]] {
      def point[A](a: => A): F[Var[IncSet[A]]] = P.newCell(IncSet.singleton(a))

      def bind[A, B](fa: F[Var[IncSet[A]]])(f: A => F[Var[IncSet[B]]]): F[Var[IncSet[B]]] =
        fa.flatMap(sa => relBind(sa)(f))
    }
}
