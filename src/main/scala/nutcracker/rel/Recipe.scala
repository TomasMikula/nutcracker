package nutcracker.rel

import nutcracker.Subscription
import nutcracker.util.{Choose, ContU}
import nutcracker.util.ops.indexedContT._
import scalaz.Bind
import scalaz.syntax.bind._
import shapeless.HList

abstract class Recipe[L <: HList, C <: HList, M[_]](val choose: Choose[L, C]) {
  // FIXME should return M[(L, Subscription[M])] instead of ContU.
  // Such implementation of Tupled2 can be accomplished when there are read-only cells.
  def create(c: C): ContU[M, (L, Subscription[M])]

  def andThen(f: L => M[Unit])(implicit M: Bind[M]): Recipe[L, C, M] = new Recipe[L, C, M](choose) {
    def create(c: C): ContU[M, (L, Subscription[M])] = Recipe.this.create(c).map(ls => f(ls._1).as(ls)).absorbEffect
  }
}
