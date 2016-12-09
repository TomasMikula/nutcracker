package nutcracker.util.ops

import scala.language.implicitConversions

import nutcracker.util.{DeepShow, Desc}

final case class DescOps[A](a: A) extends AnyVal {
  def desc[Ptr[_]](implicit ev: DeepShow[A, Ptr]): Desc[Ptr] = ev.show(a)
}

trait ToDescOps {
  implicit def toDescOps[A](a: A): DescOps[A] = DescOps(a)
}