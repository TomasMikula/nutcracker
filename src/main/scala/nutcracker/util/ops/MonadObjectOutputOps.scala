package nutcracker.util.ops

import scala.language.implicitConversions
import nutcracker.util.MonadObjectOutput

final case class MonadObjectOutputOps[M[_], S, Ptr[_]](m: M[Unit])(implicit M: MonadObjectOutput[M, S, Ptr]) {
  def ++(that: M[Unit]): M[Unit] = M.bind(m)(_ => that)
}

trait ToMonadObjectOutputOps {
  implicit def toMonadObjectOutputOps[M[_], S, Ptr[_]](m: M[Unit])(implicit M: MonadObjectOutput[M, S, Ptr]): MonadObjectOutputOps[M, S, Ptr] =
    MonadObjectOutputOps(m)
}