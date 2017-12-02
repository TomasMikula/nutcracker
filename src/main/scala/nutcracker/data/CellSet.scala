package nutcracker.data

import nutcracker.ops._
import nutcracker.util.ops.iterator._
import nutcracker.util.{ContU, DeepEqual, DeepShow, IsEqual, MonadObjectOutput}
import nutcracker.{Dom, Propagation, Subscription, UpdateResult}
import scalaz.{Applicative, Bind, Functor, IndexedContT, Monad}
import scalaz.std.list._
import scalaz.syntax.functor._
import scalaz.syntax.bind0._

/** Similar to [[IncSet]] but specialized for domain references.
  * Failed domains might be removed from this set automatically without emitting any delta.
  */
final class CellSet[Ref[_], A] private(val value: Set[Ref[A]]) extends AnyVal {
  def size: Int = value.size
  def contains(ra: Ref[A]): Boolean = value.contains(ra)
  def head: Ref[A] = value.head
  def union(that: CellSet[Ref, A]): CellSet[Ref, A] = new CellSet(this.value union that.value)
  def toList: List[Ref[A]] = value.toList
}

object CellSet {
  def apply[Ref[_], A](refs: Ref[A]*): CellSet[Ref, A] = new CellSet(Set(refs: _*))
  def empty[Ref[_], A]: CellSet[Ref, A] = new CellSet(Set())
  def singleton[Ref[_], A](ref: Ref[A]): CellSet[Ref, A] = new CellSet(Set(ref))
  def wrap[Ref[_], A](refs: Set[Ref[A]]): CellSet[Ref, A] = new CellSet(refs)

  sealed trait Update[Ref[_], A]
  private case class Insert[Ref[_], A](value: Set[Ref[A]]) extends Update[Ref, A]
  private case class RemoveFailed[Ref[_], A](ref: Ref[A]) extends Update[Ref, A]

  final case class Added[Ref[_], A](value: Set[Ref[A]]) extends AnyVal
  type Delta[Ref[_], A] = Added[Ref, A]

  type IncRefSetDom[Ref[_], A] = Dom.Aux[CellSet[Ref, A], Update[Ref, A], Delta[Ref, A]]

  implicit def domInstance[Ref[_], A]: IncRefSetDom[Ref, A] = new Dom[CellSet[Ref, A]] {
    type Update = CellSet.Update[Ref, A]
    type Delta = CellSet.Delta[Ref, A]

    override def isFailed(d: CellSet[Ref, A]): Boolean = false

    override def update[S <: CellSet[Ref, A]](s: S, u: Update): UpdateResult[CellSet[Ref, A], IDelta, S] = u match {
      case Insert(refs) =>
        val res = wrap(s.value union refs)
        if(res.size > s.size) UpdateResult(res, Added(refs diff s.value))
        else UpdateResult()
      case RemoveFailed(ref) =>
        val refs = s.value - ref
        if(refs.size < s.value.size) UpdateResult(wrap(refs), Added(Set())) // we don't publish auto-cleaned refs
        else UpdateResult()
    }

    override def appendDeltas(d1: Delta, d2: Delta): Delta =
      Added(d1.value union d2.value)
  }

  implicit def deepEqual[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[CellSet[Ptr1, A1], CellSet[Ptr2, A2], Ptr1, Ptr2] =
    new DeepEqual[CellSet[Ptr1, A1], CellSet[Ptr2, A2], Ptr1, Ptr2] {
      def equal(s1: CellSet[Ptr1, A1], s2: CellSet[Ptr2, A2]): IsEqual[Ptr1, Ptr2] = IsEqual.setEqual(s1.value, s2.value)
    }

  implicit def deepShow[Ptr[_], A](implicit ev: DeepShow[A, Ptr]): DeepShow[CellSet[Ptr, A], Ptr] =
    new DeepShow.FromSerialize[CellSet[Ptr, A], Ptr] {
      def serialize[M[_]](a: CellSet[Ptr, A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
        DeepShow.set(ev.pointer).serialize(a.value)
    }

  def init[A]: InitSyntaxHelper[A] = new InitSyntaxHelper[A]

  final class InitSyntaxHelper[A] {
    def apply[F[_], Var[_], Val[_]]()(implicit P: Propagation[F, Var, Val]): F[Var[CellSet[Var, A]]] =
      P.newCell(CellSet.empty[Var, A])
  }

  /** Returns the given set in a CPS style, executing any subsequently
    * given callback for every current and future element of that set.
    */
  def forEach[F[_], Var[_], Val[_], A](set: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], F: Applicative[F]): IndexedContT[F, Subscription[F], Unit, Var[A]] = {
    import scalaz.syntax.traverse._
    IndexedContT(f => set.observe.by(as => {
      val now = as.toList.traverse_(f)
      val onChange = P.continually((as: CellSet[Var, A], delta: Added[Var, A]) => delta.value.toList.traverse_(f))
      P.fireReload(now, onChange)
    }))
  }

  def forEach_[F[_], Var[_], Val[_], A](set: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], F: Applicative[F]): ContU[F, Var[A]] = {
    val cps = forEach(set)
    ContU(f => cps(f).void)
  }

  def insert[F[_], Var[_], Val[_], A](ref: Var[A], into: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Functor[F]): F[Unit] =
    ref.observe.by(a =>
      if(dom.isFailed(a)) P.discard
      else P.reconsider(P.update(into).by(Insert(Set(ref))) map { (_: Unit) => P.sleep(P.threshold1(a =>
        if(dom.isFailed(a)) Some(P.update(into).by(RemoveFailed(ref)))
        else None
      )) })
    ).void

  def insertAll[F[_], Var[_], Val[_], A](add: Set[Var[A]], into: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Applicative[F]): F[Unit] =
    add.iterator.traverse_(ra => insert(ra, into))

  def include[F[_], Var[_], Val[_], A](sub: Var[CellSet[Var, A]], sup: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Applicative[F]): F[Unit] =
    sub.observe.by((sa: CellSet[Var, A]) => {
      val now = insertAll(sa.value, sup)
      val onChange = P.continually((sa: CellSet[Var, A], delta: Added[Var, A]) => insertAll(delta.value, sup))
      P.fireReload(now, onChange)
    }).void

  def includeC[F[_], Var[_], Val[_], A](cps: ContU[F, Var[A]], ref: Var[CellSet[Var, A]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Functor[F]): F[Unit] =
    cps(a => insert(a, ref))

  def collect[F[_], Var[_], Val[_], A](cps: ContU[F, Var[A]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Bind[F]): F[Var[CellSet[Var, A]]] = for {
    res <- init[A]()
    _   <- includeC(cps, res)
  } yield res

  def collectAll[F[_], Var[_], Val[_], A](cps: ContU[F, Var[A]]*)(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Monad[F]): F[Var[CellSet[Var, A]]] =
    collectAll(cps)

  def collectAll[F[_], Var[_], Val[_], A](cps: Iterable[ContU[F, Var[A]]])(implicit P: Propagation[F, Var, Val], dom: Dom[A], F: Monad[F]): F[Var[CellSet[Var, A]]] =
    collect(ContU.sequence(cps))

  def relBind[F[_], Var[_], Val[_], A, B](sref: Val[CellSet[Var, A]])(f: Var[A] => F[Var[CellSet[Var, B]]])(implicit P: Propagation[F, Var, Val], domB: Dom[B], M: Monad[F]): F[Var[CellSet[Var, B]]] = {
    import scalaz.syntax.foldable0._
    for {
      res <- init[B]()
      _ <- P.observe[CellSet[Var, A]](sref).by((sa: CellSet[Var, A]) => {
        val now = sa.toList.traverse_(f(_) >>= (refb => include(refb, res)))
        val onChange = P.continually((sa: CellSet[Var, A], delta: Added[Var, A]) => delta.value.toList.traverse_(f(_) >>= (refb => include(refb, res))))
        P.fireReload(now, onChange)
      })
    } yield res
  }
}
