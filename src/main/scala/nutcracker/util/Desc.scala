package nutcracker.util

object Desc {
  def apply[Ptr[_], A](a: A)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    ev.show(a)

  def ref[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    FreeObjectOutput.writeObject(pa, ev)

  def done[Ptr[_]](s: String): Desc[Ptr] =
    FreeObjectOutput.write(s)

  def empty[Ptr[_]]: Desc[Ptr] =
    FreeObjectOutput.empty[String, Ptr]

  def setDesc[Ptr[_], A](sa: Set[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    Desc.done("{") ++ mkString(sa)(", ") ++ Desc.done("}")

  def mkString[Ptr[_], A](sa: Iterable[A])(sep: String)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] = {
    val it = sa.iterator.map(ev.show)
    join(it)(sep)
  }

  def join[Ptr[_]](descs: Iterable[Desc[Ptr]])(sep: String): Desc[Ptr] = {
    join(descs.iterator)(sep)
  }

  private def join[Ptr[_]](it: Iterator[Desc[Ptr]])(sep: String): Desc[Ptr] = {
    if (it.hasNext) {
      val h = it.next()
      it.foldLeft(h)((acc, d) => acc ++ Desc.done(sep) ++ d)
    } else
      Desc.done("")
  }
}