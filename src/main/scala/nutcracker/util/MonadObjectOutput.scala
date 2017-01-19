package nutcracker.util

import scalaz.MonadTell

trait MonadObjectOutput[F[_], R, Ptr[_]] extends MonadTell[F, R] {

  /** Alias for [[tell]]. */
  def write(r: R): F[Unit] = tell(r)

  def writeObject[A](pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): F[Unit]
}
