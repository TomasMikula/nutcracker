package nutcracker

import nutcracker.util.Choose
import shapeless.HList

trait RelChoice[V <: HList, L <: HList] {
  def rel: Rel[L]
  def choose: Choose[V, L]

  def vertexSet: Set[Int] = choose.vertexSet
  def matches[K <: HList](r: Rel[K]): Option[RelChoice[V, K]] =
    if(r == rel) Some(this.asInstanceOf[RelChoice[V, K]])
    else None
}

object RelChoice {

  def apply[V <: HList, L <: HList](
    rel0: Rel[L],
    choose0: Choose[V, L]
  ) = new RelChoice[V, L] {
    def rel: Rel[L] = rel0
    def choose: Choose[V, L] = choose0
  }
}