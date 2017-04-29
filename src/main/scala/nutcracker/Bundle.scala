package nutcracker

import nutcracker.util.{FreeK, HOrderK, ShowK}
import scala.language.implicitConversions
import scalaz.Monad

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

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): A

  override def fetch[A](ref: Val[A], s: State): A = fetchK(ref, s)
}

trait StashBundle extends Bundle with StashToolkit {
  def stashRestoreK[K[_]]: StashRestore[StateK[K]]

  override def stashRestore: StashRestore[State] = stashRestoreK[Prg]
}

trait Toolkit {
  type Prg[_]
  type State

  implicit def prgMonad: Monad[Prg]

  def empty: State
  def interpret[A](p: Prg[A], s: State): (State, A)

  def interpret0[A](p: Prg[A]): (State, A) =
    interpret(p, empty)
}

trait RefToolkit extends Toolkit {
  type Var[_]
  type Val[_]

  implicit def varOrder: HOrderK[Var]
  implicit def varShow: ShowK[Var]
  implicit def valOrder: HOrderK[Val]
  implicit def valShow: ShowK[Val]

  implicit def readOnly[A](ref: Var[A]): Val[A]

  def fetch[A](ref: Val[A], s: State): A

  def fetchResult[A](ref: Val[A], s: State)(implicit fin: Final[A]): Option[fin.Out] =
    fin.extract(fetch(ref, s))
}

trait StashToolkit extends Toolkit {
  implicit def stashRestore: StashRestore[State]
}