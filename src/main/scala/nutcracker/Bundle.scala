package nutcracker

import nutcracker.util.{FreeK, HEqualK, ShowK}

/** Bundle provides multiple APIs and is typically created by composing
  * multiple [[Module]]s.
  *
  * It implements [[Toolkit]] with [[Toolkit#Prg]] being a free monad.
  */
trait Bundle extends Toolkit {
  type Lang[K[_], A]

  override type Prg[A] = FreeK[Lang, A]
}

trait RefBundle extends Bundle with RefToolkit

trait StashBundle extends Bundle with StashToolkit

trait Toolkit {
  type Prg[_]
  type State[K[_]]

  def empty[K[_]]: State[K]
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A)

  def interpret0[A](p: Prg[A]): (State[Prg], A) =
    interpret(p, empty[Prg])
}

trait RefToolkit extends Toolkit {
  type Var[_]
  type Val[_]

  implicit def refEquality: HEqualK[Var]
  implicit def refShow: ShowK[Var]

  def fetch[K[_], A](ref: Val[A], s: State[K]): A
}

trait StashToolkit extends Toolkit {
  implicit def stashRestore[K[_]]: StashRestore[State[K]]
}