package nutcracker.toolkit

import nutcracker.util.{FreeK, HOrderK, ShowK}

/** A [[Toolkit]] whose representation of a program ([[Toolkit.Prg]]) is
  * a free monad over some algebra ([[FreeToolkit.Lang]]).
  *
  * [[FreeToolkit]] is typically created by composing multiple [[Module]]s.
  */
trait FreeToolkit extends Toolkit {
  type Lang[K[_], A]
  type StateK[K[_]]

  override type Prg[A] = FreeK[Lang, A]
  override type State = StateK[Prg]

  def emptyK[K[_]]: StateK[K]

  override def empty: State = emptyK[Prg]
}

trait FreeRefToolkit extends FreeToolkit with RefToolkit {
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

trait FreeStashToolkit extends FreeToolkit with StashToolkit {
  def stashRestoreK[K[_]]: StashRestore[StateK[K]]

  override def stashRestore: StashRestore[State] = stashRestoreK[Prg]
}