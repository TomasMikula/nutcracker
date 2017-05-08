package nutcracker.toolkit

import nutcracker.rel.Relations
import nutcracker.util.{FreeK, Inject, Lst, Step, WriterState}
import nutcracker.util.ops._
import scalaz.Lens

trait RelModule extends Module {
  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): Step[K, Lang[K, ?], S]
  def freeRelations[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Relations[FreeK[F, ?]]
}

object RelModule {
  val instance: PersistentRelModule = RelModuleImpl
}

trait PersistentRelModule extends RelModule with PersistentStateModule { self =>
  override def stashable: StashRelModule { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentRelModule {
  type Aux[Lang0[_[_], _], State0[_[_]]] = PersistentRelModule {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

trait StashRelModule extends RelModule with StashModule

private[toolkit] object RelModuleImpl extends PersistentRelModule {
  type Lang[K[_], A] = RelLang[K, A]
  type StateK[K[_]] = RelDB[K]

  def emptyK[K[_]]: StateK[K] = RelDB.empty[K]

  override def freeRelations[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Relations[FreeK[F, ?]] =
    RelLang.relationsInstance[F]

  def interpreter[K[_], S](implicit lens: Lens[S, RelDB[K]]): Step[K, RelLang[K, ?], S] = new Step[K, RelLang[K, ?], S] {
    import RelLang._

    override def apply[A](f: RelLang[K, A]): WriterState[Lst[K[Unit]], S, A] = f match {
      case r @ Relate(rel, values) =>
        WriterState(s => lens.get(s).insert(rel, values)(r.ordersWitness, r.orders) match { case (db1, ks) => (ks, s set db1, ()) })
      case OnPatternMatch(p, a, h) =>
        WriterState(s => lens.get(s).addOnPatternMatch(p, a)(h) match { case (db1, ks) => (ks, s set db1, ()) })
      case ExecWith(rel, ass, supp, exec, m, os) =>
        WriterState(s => lens.get(s).execWith(rel, ass)(supp)(exec)(m, os) match { case (db1, ko) => (Lst.maybe(ko), s set db1, ()) })
      case Supply(rel, token, value) =>
        WriterState(s => lens.get(s).supply(rel, token, value) match { case (db1, ks) => (ks, s set db1, ()) })
    }
  }

  def stashable = new RelListModule[Lang, StateK](this)
}

private[toolkit] class RelListModule[Lang[_[_], _], State0[_[_]]](base: PersistentRelModule.Aux[Lang, State0]) extends ListModule[Lang, State0](base) with StashRelModule {
  def freeRelations[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]) = base.freeRelations[F]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): Step[K, Lang[K, ?], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))
}