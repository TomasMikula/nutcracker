package nutcracker.toolkit

import nutcracker.util.{FreeK, HOrderK, ShowK}

/** Bundle provides multiple APIs and is typically created by composing
  * multiple [[Module]]s.
  *
  * It implements [[Toolkit]] with [[Toolkit#Prg]] being a free monad.
  */
trait Bundle extends Toolkit {
  type Lang[K[_], A]
  type StateK[K[_]]

  override type Prg[A] = FreeK[Lang, A]
  override type State = StateK[Prg]

  def emptyK[K[_]]: StateK[K]

  override def empty: State = emptyK[Prg]
}

trait RefBundle extends Bundle with RefToolkit {
  type VarK[K[_], A]
  type ValK[K[_], A]

  type Var[A] = VarK[Prg, A]
  type Val[A] = ValK[Prg, A]

  def varOrderK[K[_]]: HOrderK[VarK[K, ?]]
  def varShowK[K[_]]:  ShowK[VarK[K, ?]]
  def valOrderK[K[_]]: HOrderK[ValK[K, ?]]
  def valShowK[K[_]]:  ShowK[ValK[K, ?]]

  implicit def varOrder: HOrderK[Var] = varOrderK[Prg]
  implicit def varShow:  ShowK[Var]   = varShowK[Prg]
  implicit def valOrder: HOrderK[Val] = valOrderK[Prg]
  implicit def valShow:  ShowK[Val]   = valShowK[Prg]

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A]
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A

  override def fetch[A](ref: Val[A], s: State): Option[A] = fetchK(ref, s)
  override def fetch[A](ref: Var[A], s: State): A = fetchK(ref, s)
}

trait StashBundle extends Bundle with StashToolkit {
  def stashRestoreK[K[_]]: StashRestore[StateK[K]]

  override def stashRestore: StashRestore[State] = stashRestoreK[Prg]
}