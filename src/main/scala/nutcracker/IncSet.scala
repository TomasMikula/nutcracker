package nutcracker

import scala.language.higherKinds
import nutcracker.PropagationLang._
import nutcracker.util.{ContF, FreeK, InjectK}

import scalaz.Monad

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

  type IncSetDom[A] = Dom .Aux[IncSet[A], Join[IncSet[A]], Diff[Set[A]]]
  type IncSetRef[A] = DRef.Aux[IncSet[A], Join[IncSet[A]], Diff[Set[A]]]

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

  def init[A]: FreeK[PropagationLang, IncSetRef[A]] = cellF(IncSet.empty[A])
  def initF[F[_[_], _], A](implicit inj: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[A]] =
    init[A].inject[F]

  /** Returns the given set in a CPS style, executing any subsequently
    * given callback for every current and future element of that set.
    */
  def forEach[F[_[_], _], A](ref: IncSetRef[A])(implicit inj: InjectK[PropagationLang, F]): ContF[F, A] =
    ContF(f => domTriggerF(ref)(as => {
      val now = FreeK.sequence_(as.toList.map(f))
      val onChange = (as: IncSet[A], delta: Diff[Set[A]]) => Trigger.fireReload(FreeK.sequence_(delta.value.toList.map(f)))
      (Some(now), Some(onChange))
    }))

  def insert[A](a: A, into: IncSetRef[A]): FreeK[PropagationLang, Unit] =
    insertAll(Set(a), into)

  def insertAll[A](add: Set[A], into: IncSetRef[A]): FreeK[PropagationLang, Unit] =
    PropagationLang.updateF(into)(Join(wrap(add)))

  def include[A](sub: IncSetRef[A], sup: IncSetRef[A]): FreeK[PropagationLang, Unit] =
    domTriggerF(sub)((sa: IncSet[A]) => {
      val now = Some(insertAll(sa.value, sup))
      val onChange = Some((sa: IncSet[A], delta: Diff[Set[A]]) => FireReload(insertAll(delta.value, sup)))
      (now, onChange)
    })

  def includeC[F[_[_], _], A](cps: ContF[F, A], ref: IncSetRef[A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    cps(a => IncSet.insert(a, ref).inject[F])

  def collect[F[_[_], _], A](cps: ContF[F, A])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[A]] = for {
    res <- IncSet.initF[F, A]
    _   <- includeC(cps, res)
  } yield res

  def collectAll[F[_[_], _], A](cps: ContF[F, A]*)(implicit inj: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[A]] =
    collectAll(cps)

  def collectAll[F[_[_], _], A](cps: Iterable[ContF[F, A]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[A]] =
    collect(ContF.sequence(cps))

  /** Relative monadic bind. [[nutcracker.IncSet.IncSetRef]] is a monad relative to `FreeK[F, ?]`,
    * i.e. we can implement `bind` if additional effects of type `FreeK[F, ?]` are allowed.
    * This is equivalent to having a monad instance for `λ[A => FreeK[F, IncSetRef[A]]]`.
    */
  def relBind[F[_[_], _], A, B](sref: IncSetRef[A])(f: A => FreeK[F, IncSetRef[B]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, IncSetRef[B]] = for {
    res <- initF[F, B]
    _ <- domTriggerF[F, IncSet[A]](sref)((sa: IncSet[A]) => {
      val now = FreeK.sequence_(sa.toList.map(f(_) >>= (refb => include(refb, res))))
      val onChange = Some((sa: IncSet[A], delta: Diff[Set[A]]) => FireReload(FreeK.sequence_(delta.value.toList.map(f(_) >>= (refb => include(refb, res))))))
      (Some(now), onChange)
    })
  } yield res

  implicit def monad[F[_[_], _]](implicit inj: InjectK[PropagationLang, F]): Monad[λ[A => FreeK[F, IncSetRef[A]]]] =
    new Monad[λ[A => FreeK[F, IncSetRef[A]]]] {
      def point[A](a: => A): FreeK[F, IncSetRef[A]] = cellF(singleton(a)).inject[F]

      def bind[A, B](fa: FreeK[F, IncSetRef[A]])(f: A => FreeK[F, IncSetRef[B]]): FreeK[F, IncSetRef[B]] =
        fa.flatMap(sa => relBind(sa)(f))
    }
}
