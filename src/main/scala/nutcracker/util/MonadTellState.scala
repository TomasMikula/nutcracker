package nutcracker.util

import scalaz.{MonadState, MonadTell}

trait MonadTellState[F[_], W, S] extends MonadTell[F, W] with MonadState[F, S] {
  def writerState[A](f: S => (W, S, A)): F[A]
}