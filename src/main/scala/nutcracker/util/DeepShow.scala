package nutcracker.util

import scalaz.{Show, ~>}
import scalaz.Id._

object DeepShow {

  type FromFree[A, Ptr[_]] = ObjectSerializer.FromFree[A, String, Ptr]

  type FromWrite[A, Ptr[_]] = ObjectSerializer.FromWrite[A, String, Ptr]

  type FromSerialize[A, Ptr[_]] = ObjectSerializer.FromSerialize[A, String, Ptr]

  def show[Ptr[_], A](deref: Ptr ~> Id)(implicit ev: DeepShow[A, Ptr], S: ShowK[Ptr], E: HEqualK[Ptr]): Show[A] =
    ev.show(deref, S)()
}

trait DeepShowK[A[_[_]]] {
  def show[Ptr[_]](a: A[Ptr]): Desc[Ptr]

  def specialize[Ptr[_]]: DeepShow[A[Ptr], Ptr] = new DeepShow.FromFree[A[Ptr], Ptr] {
    def free(a: A[Ptr]): Desc[Ptr] = DeepShowK.this.show(a)
  }
}



