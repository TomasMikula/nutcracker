package nutcracker

import scala.language.higherKinds
import nutcracker.util.{ContU, DeepEqual, DeepShow, IsEqual, MonadObjectOutput}

import scalaz.{Applicative, Bind, Functor, Monad}
import scalaz.std.list._
import scalaz.syntax.bind._

/** Similar to [[IncSet]] but specialized for domain references.
  * Failed domains might be removed from this set automatically without emitting a delta.
  */
final class IncRefSet[Ref[_], A] private(val value: Set[Ref[A]]) extends AnyVal {
  def size: Int = value.size
  def contains(ra: Ref[A]): Boolean = value.contains(ra)
  def head: Ref[A] = value.head
  def union(that: IncRefSet[Ref, A]): IncRefSet[Ref, A] = new IncRefSet(this.value union that.value)
  def toList: List[Ref[A]] = value.toList
}

object IncRefSet {
  def apply[Ref[_], A](refs: Ref[A]*): IncRefSet[Ref, A] = new IncRefSet(Set(refs: _*))
  def empty[Ref[_], A]: IncRefSet[Ref, A] = new IncRefSet(Set())
  def singleton[Ref[_], A](ref: Ref[A]): IncRefSet[Ref, A] = new IncRefSet(Set(ref))
  def wrap[Ref[_], A](refs: Set[Ref[A]]): IncRefSet[Ref, A] = new IncRefSet(refs)

  type Update[Ref[_], A] = Join[IncRefSet[Ref, A]]
  type Delta[Ref[_], A] = Diff[Set[Ref[A]]]

  type IncRefSetDom[Ref[_], A] = Dom.Aux[IncRefSet[Ref, A], Join[IncRefSet[Ref, A]], Diff[Set[Ref[A]]]]

  implicit def domInstance[Ref[_], A]: IncRefSetDom[Ref, A] = new Dom[IncRefSet[Ref, A]] {
    type Update = IncRefSet.Update[Ref, A]
    type Delta = IncRefSet.Delta[Ref, A]

    override def assess(d: IncRefSet[Ref, A]): Dom.Status[Join[IncRefSet[Ref, A]]] = d.size match {
      case 0 => Dom.Unrefined(() => None)
      case _ => Dom.Refined
    }

    override def update(s: IncRefSet[Ref, A], u: Join[IncRefSet[Ref, A]]): Option[(IncRefSet[Ref, A], Diff[Set[Ref[A]]])] = {
      val res = s union u.value
      if(res.size > s.size) Some((res, Diff(u.value.value diff s.value)))
      else None
    }

    override def appendDeltas(d1: Diff[Set[Ref[A]]], d2: Diff[Set[Ref[A]]]): Diff[Set[Ref[A]]] =
      Diff(d1.value union d2.value)
  }

  implicit def deepEqual[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[IncRefSet[Ptr1, A1], IncRefSet[Ptr2, A2], Ptr1, Ptr2] =
    new DeepEqual[IncRefSet[Ptr1, A1], IncRefSet[Ptr2, A2], Ptr1, Ptr2] {
      def equal(s1: IncRefSet[Ptr1, A1], s2: IncRefSet[Ptr2, A2]): IsEqual[Ptr1, Ptr2] = IsEqual.setEqual(s1.value, s2.value)
    }

  implicit def deepShow[Ptr[_], A](implicit ev: DeepShow[A, Ptr]): DeepShow[IncRefSet[Ptr, A], Ptr] =
    new DeepShow.FromSerialize[IncRefSet[Ptr, A], Ptr] {
      def serialize[M[_]](a: IncRefSet[Ptr, A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
        DeepShow.set(ev.pointer).serialize(a.value)
    }
}


class IncRefSets[F[_], Ref[_]](implicit P: Propagation[F, Ref]) {

  def init[A]: F[Ref[IncRefSet[Ref, A]]] =
    P.newCell(IncRefSet.empty[Ref, A])

  /** Returns the given set in a CPS style, executing any subsequently
    * given callback for every current and future element of that set.
    */
  def forEach[A](ref: Ref[IncRefSet[Ref, A]])(implicit A: Applicative[F]): ContU[F, Ref[A]] = {
    import scalaz.syntax.traverse._
    ContU(f => P.observe(ref).by(as => {
      val now = as.toList.traverse_(f)
      val onChange = Trigger.continually((as: IncRefSet[Ref, A], delta: Diff[Set[Ref[A]]]) => delta.value.toList.traverse_(f))
      Trigger.fireReload(now map (_ => onChange))
    }))
  }

  def insert[A](ref: Ref[A], into: Ref[IncRefSet[Ref, A]]): F[Unit] =
    insertAll(Set(ref), into)

  def insertAll[A](add: Set[Ref[A]], into: Ref[IncRefSet[Ref, A]]): F[Unit] =
    P.update(into).by(Join(IncRefSet.wrap(add)))

  def include[A](sub: Ref[IncRefSet[Ref, A]], sup: Ref[IncRefSet[Ref, A]])(implicit F: Functor[F]): F[Unit] =
    P.observe(sub).by((sa: IncRefSet[Ref, A]) => {
      val now = insertAll(sa.value, sup)
      val onChange = Trigger.continually((sa: IncRefSet[Ref, A], delta: Diff[Set[Ref[A]]]) => insertAll(delta.value, sup))
      Trigger.fireReload(now map (_ => onChange))
    })

  def includeC[A](cps: ContU[F, Ref[A]], ref: Ref[IncRefSet[Ref, A]]): F[Unit] =
    cps(a => insert(a, ref))

  def collect[A](cps: ContU[F, Ref[A]])(implicit B: Bind[F]): F[Ref[IncRefSet[Ref, A]]] = for {
    res <- init[A]
    _   <- includeC(cps, res)
  } yield res

  def collectAll[A](cps: ContU[F, Ref[A]]*)(implicit M: Monad[F]): F[Ref[IncRefSet[Ref, A]]] =
    collectAll(cps)

  def collectAll[A](cps: Iterable[ContU[F, Ref[A]]])(implicit M: Monad[F]): F[Ref[IncRefSet[Ref, A]]] =
    collect(ContU.sequence(cps))

  def relBind[A, B](sref: Ref[IncRefSet[Ref, A]])(f: Ref[A] => F[Ref[IncRefSet[Ref, B]]])(implicit M: Monad[F]): F[Ref[IncRefSet[Ref, B]]] = {
    import scalaz.syntax.traverse._
    for {
      res <- init[B]
      _ <- P.observe[IncRefSet[Ref, A]](sref).by((sa: IncRefSet[Ref, A]) => {
        val now = sa.toList.traverse_(f(_) >>= (refb => include(refb, res)))
        val onChange = Trigger.continually((sa: IncRefSet[Ref, A], delta: Diff[Set[Ref[A]]]) => delta.value.toList.traverse_(f(_) >>= (refb => include(refb, res))))
        Trigger.fireReload(now map (_ => onChange))
      })
    } yield res
  }
}
