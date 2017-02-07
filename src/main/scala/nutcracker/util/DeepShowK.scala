package nutcracker.util

trait DeepShowK[A[_[_]]] {
  def show[Ptr[_], M[_]](a: A[Ptr])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit]

  def specialize[Ptr[_]]: DeepShow[A[Ptr], Ptr] = new DeepShow.FromSerialize[A[Ptr], Ptr] {
    def serialize[M[_]](a: A[Ptr])(implicit ev: MonadObjectOutput[M, String, Ptr]): M[Unit] =
      DeepShowK.this.show(a)
  }
}
