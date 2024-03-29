package nutcracker.toolkit

import nutcracker.util.{FreeK, HOrderK, Id, Lst, ShowK, StateInterpreter, TwoLevel, WriterState, WriterStateT}
import scala.language.implicitConversions
import scalaz.{StateT, ~>}

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

  val stepInterpreter: StateInterpreter[Prg, Lang[Prg, *], State]

  lazy val interpreter: Prg ~> StateT[State, Id, *] = {
    val freeInterpreter = stepInterpreter.free[WriterState[TwoLevel[Lst, Prg[Unit]], State, *], TwoLevel[Lst, Prg[Unit]]](WriterStateT.monadTellStateInstance[Id, TwoLevel[Lst, Prg[Unit]], State], TwoLevel.stratifiedMonoidAggregator[Lst, Prg[Unit]], WriterStateT.bindRec[Id, TwoLevel[Lst, Prg[Unit]], State], implicitly, implicitly)
    val freeKInterpreter =
      new (Prg ~> WriterState[TwoLevel[Lst, Prg[Unit]], State, *]) {
        override def apply[A](pa: Prg[A]): WriterState[TwoLevel[Lst, Prg[Unit]], State, A] =
          freeInterpreter(pa.unwrap)
      }
    WriterStateT.recurse[Prg, Id, TwoLevel[Lst, *], Prg[Unit], State](freeKInterpreter)(identity[Prg[Unit]])
  }

  def interpret[A](p: Prg[A], s: State): (State, A) = interpreter(p).run(s).value
}

trait FreeRefToolkit extends FreeToolkit with RefToolkit {
  type VarK[K[_], A]
  type ValK[K[_], A]
  type OutK[K[_], A]

  override type Var[A] = VarK[Prg, A]
  override type Val[A] = ValK[Prg, A]
  override type Out[A] = OutK[Prg, A]

  def varOrderK[K[_]]: HOrderK[VarK[K, *]]
  def varShowK[K[_]]:  ShowK[VarK[K, *]]
  def valOrderK[K[_]]: HOrderK[ValK[K, *]]
  def valShowK[K[_]]:  ShowK[ValK[K, *]]

  implicit def varOrder: HOrderK[Var] = varOrderK[Prg]
  implicit def varShow:  ShowK[Var]   = varShowK[Prg]
  implicit def valOrder: HOrderK[Val] = valOrderK[Prg]
  implicit def valShow:  ShowK[Val]   = valShowK[Prg]

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A]
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A
  def readOutK[K[_], A](a: OutK[K, A], s: StateK[K]): A

  override def fetch[A](ref: Val[A], s: State): Option[A] = fetchK(ref, s)
  override def fetch[A](ref: Var[A], s: State): A = fetchK(ref, s)
  override def readOut[A](a: Out[A], s: State): A = readOutK(a, s)

  def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A]

  override implicit def readOnly[A](ref: Var[A]): Val[A] = readOnlyK(ref)
}

trait FreeStashToolkit extends FreeToolkit with StashToolkit {
  def stashRestoreK[K[_]]: StashRestore[StateK[K]]

  override def stashRestore: StashRestore[State] = stashRestoreK[Prg]
}