package nutcracker.util

import scalaz.MonadTell

trait MonadObjectOutput[F[_], R, Ptr[_]] extends MonadTell[F, R] { self =>

  /** Alias for [[tell]]. */
  def write(r: R): F[Unit] = tell(r)

  def writeObject[A](pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): F[Unit]

  def objectOutput: ObjectOutput[F[Unit], R, Ptr] = new ObjectOutput[F[Unit], R, Ptr] {
    def write(out: F[Unit], r: R): F[Unit] = self.bind(out)((_: Unit) => self.write(r))

    def writeObject[A](out: F[Unit], pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): F[Unit] =
      self.bind(out)((_: Unit) => self.writeObject(pa))
  }
}
