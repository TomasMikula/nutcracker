package nutcracker.util

import scalaz.{BindRec, MonadTell}

trait MonadObjectOutput[F[_], R, Ptr[_]] extends MonadTell[F, R] with BindRec[F] { self =>

  /** Alias for [[tell]]. */
  def write(r: R): F[Unit]

  override def tell(r: R): F[Unit] = write(r)

  def writeRec[A](pa: Ptr[A])(f: A => F[Unit]): F[Unit]

  def writeObject[A](pa: Ptr[A])(implicit ev: ObjectSerializer[A, R, Ptr]): F[Unit] =
    writeRec(pa)(ev.serialize[F](_)(self))

  def writeSubObject[A, B](pa: Ptr[A])(f: A => B)(implicit ev: ObjectSerializer[B, R, Ptr]): F[Unit] =
    writeRec(pa)(a => ev.serialize[F](f(a))(self))

  def apply[A](a: A)(implicit ev: ObjectSerializer[A, R, Ptr]): F[Unit] =
    ev.serialize(a)(self)

  def empty: F[Unit] = point(())

  /** A hint for displaying tree structure.
    * The implementation is identity, which means ignoring the hint, i.e. no support for displaying tree structure.
    */
  def nest[A](fa: F[A]): F[A]
  // TODO: can we do an analog of nest for ObjectOutput?

  def objectOutput: ObjectOutput[F[Unit], R, Ptr] = new ObjectOutput[F[Unit], R, Ptr] {
    def write(out: F[Unit], r: R): F[Unit] = self.bind(out)((_: Unit) => self.write(r))

    def writeSubObject[A, B](out: F[Unit], pa: Ptr[A])(f: A => B)(implicit ser: ObjectSerializer[B, R, Ptr]): F[Unit] =
      self.bind(out)((_: Unit) => self.writeRec(pa)(a => ser.serialize[F](f(a))(self)))
  }
}