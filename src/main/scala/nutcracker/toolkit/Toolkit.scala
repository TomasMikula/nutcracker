package nutcracker.toolkit

import nutcracker.Final
import nutcracker.util.{HOrderK, ShowK}
import scala.language.implicitConversions
import scalaz.{Foldable, Monad}

/** Provides implementations of multiple APIs.
  * The specific APIs provided by a [[Toolkit]] are defined in subclasses,
  * such as [[PropagationToolkit]], [[BranchingToolkit]], [[RelToolkit]], etc.
  *
  * The unifying feature across all provided APIs is that programs written
  * against those APIs can be represented by a type [[Toolkit.Prg]],
  * which is at least a monad. Such programs can be interpreted
  * ([[Toolkit.interpret[A]*]]) as state transitions on [[Toolkit.State]].
  */
trait Toolkit {
  type Prg[_]
  type State

  implicit def prgMonad: Monad[Prg]

  def empty: State
  def interpret[A](p: Prg[A], s: State): (State, A)

  def interpret0[A](p: Prg[A]): (State, A) =
    interpret(p, empty)

  def interpretAll[F[_]](ps: F[Prg[Unit]], s: State)(implicit F: Foldable[F]): State =
    F.foldLeft(ps, s)((s, p) => interpret(p, s)._1)
}

/** Extends [[Toolkit]] with a notion of observable ([[RefToolkit.Val]])
  * and writable ([[RefToolkit.Var]]) cells, (potentially) residing inside [[RefToolkit.State]].
  */
trait RefToolkit extends Toolkit {
  type Var[_]
  type Val[_]

  implicit def varOrder: HOrderK[Var]
  implicit def varShow: ShowK[Var]
  implicit def valOrder: HOrderK[Val]
  implicit def valShow: ShowK[Val]

  implicit def readOnly[A](ref: Var[A]): Val[A]

  def fetch[A](ref: Val[A], s: State): Option[A]
  def fetch[A](ref: Var[A], s: State): A

  def fetchResult[A](ref: Val[A], s: State)(implicit fin: Final[A]): Option[fin.Out] =
    fetch(ref, s).flatMap(fin.extract(_))
}

/** When [[StashToolkit.State]] is not a persistent data structure, it might
  * still provide a way to remember the current state (as if push on the stack)
  * and return to it later (as if pop off the stack). [[StashToolkit]] is a
  * [[Toolkit]] that supports such operations on [[Toolkit.State]].
  */
trait StashToolkit extends Toolkit {
  implicit def stashRestore: StashRestore[State]
}