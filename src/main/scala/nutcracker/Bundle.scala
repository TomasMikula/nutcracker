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

  override type Prg[A] = FreeK[Lang, A]
}

trait RefBundle extends Bundle with RefToolkit

trait StashBundle extends Bundle with StashToolkit

trait Toolkit {
  type Prg[_]
  type State[K[_]]

  implicit def prgMonad: Monad[Prg]

  def empty[K[_]]: State[K]
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A)

  def interpret0[A](p: Prg[A]): (State[Prg], A) =
    interpret(p, empty[Prg])
}

trait RefToolkit extends Toolkit {
  type Var[_]
  type Val[_]

  implicit def varOrder: HOrderK[Var]
  implicit def varShow: ShowK[Var]
  implicit def valOrder: HOrderK[Val]
  implicit def valShow: ShowK[Val]

  implicit def readOnly[A](ref: Var[A]): Val[A]

  def fetch[K[_], A](ref: Val[A], s: State[K]): A

  def fetchResult[K[_], A](ref: Val[A], s: State[K])(implicit fin: Final[A]): Option[fin.Out] =
    fin.extract(fetch(ref, s))
}

trait StashToolkit extends Toolkit {
  implicit def stashRestore[K[_]]: StashRestore[State[K]]
}