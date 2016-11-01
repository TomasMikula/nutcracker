package nutcracker

import scala.language.higherKinds
import nutcracker.util.ContU

import scalaz.{Applicative, Bind, Monad}
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

  type Update[A] = Join[IncSet[A]]
  type Delta[A] = Diff[Set[A]]

  type IncSetDom[A] = Dom.Aux[IncSet[A], Join[IncSet[A]], Diff[Set[A]]]

  implicit def domInstance[A]: IncSetDom[A] = new Dom[IncSet[A]] {
    type Update = IncSet.Update[A]
    type Delta = IncSet.Delta[A]

    override def assess(d: IncSet[A]): Dom.Status[Join[IncSet[A]]] = d.size match {
      case 0 => Dom.Unrefined(() => None)
      case _ => Dom.Refined
    }

    override def update(s: IncSet[A], u: Join[IncSet[A]]): Option[(IncSet[A], Diff[Set[A]])] = {
      val res = s union u.value
      if(res.size > s.size) Some((res, Diff(u.value.value diff s.value)))
      else None
    }

    override def combineDeltas(d1: Diff[Set[A]], d2: Diff[Set[A]]): Diff[Set[A]] =
      Diff(d1.value union d2.value)
  }

  def init[F[_], Ref[_], A](implicit P: Propagation[F, Ref]): F[Ref[IncSet[A]]] =
    P.cell(IncSet.empty[A])

  /** Returns the given set in a CPS style, executing any subsequently
    * given callback for every current and future element of that set.
    */
  def forEach[F[_], Ref[_], A](ref: Ref[IncSet[A]])(implicit P: Propagation[F, Ref], A: Applicative[F]): ContU[F, A] = {
    import scalaz.syntax.traverse._
    ContU(f => P.observe(ref).by(as => {
      val now = as.toList.traverse_(f)
      val onChange = (as: IncSet[A], delta: Diff[Set[A]]) => Trigger.fireReload(delta.value.toList.traverse_(f))
      (Some(now), Some(onChange))
    }))
  }

  def insert[F[_], Ref[_], A](a: A, into: Ref[IncSet[A]])(implicit P: Propagation[F, Ref]): F[Unit] =
    insertAll(Set(a), into)

  def insertAll[F[_], Ref[_], A](add: Set[A], into: Ref[IncSet[A]])(implicit P: Propagation[F, Ref]): F[Unit] =
    P.update(into).by(Join(wrap(add)))

  def include[F[_], Ref[_], A](sub: Ref[IncSet[A]], sup: Ref[IncSet[A]])(implicit P: Propagation[F, Ref]): F[Unit] =
    P.observe(sub).by((sa: IncSet[A]) => {
      val now = Some(insertAll(sa.value, sup))
      val onChange = Some((sa: IncSet[A], delta: Diff[Set[A]]) => FireReload(insertAll(delta.value, sup)))
      (now, onChange)
    })

  def includeC[F[_], Ref[_], A](cps: ContU[F, A], ref: Ref[IncSet[A]])(implicit P: Propagation[F, Ref]): F[Unit] =
    cps(a => IncSet.insert(a, ref))

  def collect[F[_]: Bind, Ref[_], A](cps: ContU[F, A])(implicit P: Propagation[F, Ref]): F[Ref[IncSet[A]]] = for {
    res <- IncSet.init[F, Ref, A]
    _   <- includeC(cps, res)
  } yield res

  def collectAll[F[_]: Monad, Ref[_], A](cps: ContU[F, A]*)(implicit P: Propagation[F, Ref]): F[Ref[IncSet[A]]] =
    collectAll(cps)

  def collectAll[F[_]: Monad, Ref[_], A](cps: Iterable[ContU[F, A]])(implicit P: Propagation[F, Ref]): F[Ref[IncSet[A]]] =
    collect(ContU.sequence(cps))

  /** Relative monadic bind. `Ref[IncSet[A]]` is a monad relative to `FreeK[F, ?]`,
    * i.e. we can implement `bind` if additional effects of type `FreeK[F, ?]` are allowed.
    * This is equivalent to having a monad instance for `λ[A => FreeK[F, IncSetRef[A]]]`.
    */
  def relBind[F[_], Ref[_], A, B](sref: Ref[IncSet[A]])(f: A => F[Ref[IncSet[B]]])(implicit P: Propagation[F, Ref], M: Monad[F]): F[Ref[IncSet[B]]] = {
    import scalaz.syntax.traverse._
    for {
      res <- init[F, Ref, B]
      _ <- P.observe[IncSet[A]](sref).by((sa: IncSet[A]) => {
        val now = sa.toList.traverse_(f(_) >>= (refb => include(refb, res)))
        val onChange = Some((sa: IncSet[A], delta: Diff[Set[A]]) => FireReload(delta.value.toList.traverse_(f(_) >>= (refb => include(refb, res)))))
        (Some(now), onChange)
      })
    } yield res
  }

  implicit def monad[F[_], Ref[_]](implicit P: Propagation[F, Ref], M: Monad[F]): Monad[λ[A => F[Ref[IncSet[A]]]]] =
    new Monad[λ[A => F[Ref[IncSet[A]]]]] {
      def point[A](a: => A): F[Ref[IncSet[A]]] = P.cell(singleton(a))

      def bind[A, B](fa: F[Ref[IncSet[A]]])(f: A => F[Ref[IncSet[B]]]): F[Ref[IncSet[B]]] =
        fa.flatMap(sa => relBind(sa)(f))
    }
}
