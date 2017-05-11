package nutcracker.toolkit

import nutcracker.rel.Relations
import nutcracker.util.{FreeK, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops._
import scalaz.{Bind, Lens}

trait RelModule extends Module {
  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S]
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

  def interpreter[K[_], S](implicit lens: Lens[S, RelDB[K]]): StateInterpreter[K, RelLang[K, ?], S] = new StateInterpreter[K, RelLang[K, ?], S] {
    import RelLang._

    def apply[M[_], W, A](fa: RelLang[K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[RelLang[K, ?], K], K: Bind[K]): M[A] = fa match {
      case r @ Relate(rel, values) =>
        M.writerState(s => lens.get(s).insert(rel, values)(r.ordersWitness, r.orders) match { case (db1, ks) => (ks at 0, s set db1, ()) })
      case OnPatternMatch(p, a, h) =>
        M.writerState(s => lens.get(s).addOnPatternMatch(p, a)(h) match { case (db1, ks) => (ks at 0, s set db1, ()) })
      case ExecWith(rel, ass, supp, exec, m, os) =>
        M.writerState(s => lens.get(s).execWith(rel, ass)(supp)(exec)(m, os) match { case (db1, ko) => (Lst.maybe(ko) at 0, s set db1, ()) })
      case Supply(rel, token, value) =>
        M.writerState(s => lens.get(s).supply(rel, token, value) match { case (db1, ks) => (ks at 0, s set db1, ()) })
    }
  }

  def stashable = new RelListModule[Lang, StateK](this)
}

private[toolkit] class RelListModule[Lang[_[_], _], State0[_[_]]](base: PersistentRelModule.Aux[Lang, State0]) extends ListModule[Lang, State0](base) with StashRelModule {
  def freeRelations[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]) = base.freeRelations[F]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))
}