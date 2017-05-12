package nutcracker

import nutcracker.util.Choose
import shapeless.HList

final case class RelChoice[V <: HList, L <: HList](rel: Rel[L], choose: Choose[V, L]) {
  def vertexSet: Set[Int] = choose.vertexSet
  def matches[K <: HList](r: Rel[K]): Option[RelChoice[V, K]] =
    if(r == rel) Some(this.asInstanceOf[RelChoice[V, K]])
    else None
}