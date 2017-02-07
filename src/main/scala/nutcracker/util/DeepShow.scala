package nutcracker.util

import scalaz.{MonadTell, Show, ~>}
import scalaz.Id._
import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.syntax.monad._

object DeepShow {

  type FromWrite[A, Ptr[_]] = ObjectSerializer.FromWrite[A, String, Ptr]

  type FromSerialize[A, Ptr[_]] = ObjectSerializer.FromSerialize[A, String, Ptr]

  def show[Ptr[_], A](deref: Ptr ~> Id)(implicit ev: DeepShow[A, Ptr], S: ShowK[Ptr], E: HEqualK[Ptr]): Show[A] =
    ev.show(deref, S)()

  def set[Ptr[_], A](implicit ev: DeepShow[A, Ptr]): DeepShow[Set[A], Ptr] = new DeepShow.FromSerialize[Set[A], Ptr] {
    def serialize[M[_]](a: Set[A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      showSet(a)
  }

  private def showSet[M[_], Ptr[_], A](sa: Set[A])(implicit ev: DeepShow[A, Ptr], M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
    List(M.write("{"), mkString(sa)(", "), M.write("}")).sequence_

  private def mkString[M[_], Ptr[_], A](sa: Iterable[A])(sep: String)(implicit ev: DeepShow[A, Ptr], M: MonadObjectOutput[M, String, Ptr]): M[Unit] = {
    val it = sa.iterator.map(ev.serialize[M])
    join(it)(sep)
  }

  def join[M[_]](descs: Iterable[M[Unit]])(sep: String)(implicit M: MonadTell[M, String]): M[Unit] = {
    join(descs.iterator)(sep)
  }

  private def join[M[_]](it: Iterator[M[Unit]])(sep: String)(implicit M: MonadTell[M, String]): M[Unit] = {
    if (it.hasNext) {
      val h = it.next()
      it.foldLeft(h)((acc, m) => acc >> M.tell(sep) >> m)
    } else
      M.point(())
  }
}

