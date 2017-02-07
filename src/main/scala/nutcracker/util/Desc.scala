package nutcracker.util

object Desc {
  def apply[Ptr[_], A](a: A)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    ev.free(a)

  def ref[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    FreeObjectOutput.writeObject(pa, ev)

  def ref[Ptr[_], A, B](pa: Ptr[A], f: A => B)(implicit ev: DeepShow[B, Ptr]): Desc[Ptr] =
    FreeObjectOutput.writeRec(pa, (a: A) => ev.free(f(a)))

  def done[Ptr[_]](s: String): Desc[Ptr] =
    FreeObjectOutput.write(s)

  def empty[Ptr[_]]: Desc[Ptr] =
    FreeObjectOutput.empty[String, Ptr]

  def nest[Ptr[_]](that: Desc[Ptr]): Desc[Ptr] =
    FreeObjectOutput.nest(that)
}