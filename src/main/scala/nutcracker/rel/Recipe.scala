package nutcracker.rel

import nutcracker.Subscription
import nutcracker.util.Choose
import scalaz.Bind
import scalaz.syntax.bind._
import shapeless.HList

abstract class Recipe[L <: HList, C <: HList, M[_]](val choose: Choose[L, C]) {
  def create(c: C): M[(L, Subscription[M])]

  def andThen(f: L => M[Unit])(implicit M: Bind[M]): Recipe[L, C, M] = new Recipe[L, C, M](choose) {
    def create(c: C): M[(L, Subscription[M])] = Recipe.this.create(c) >>= (ls => f(ls._1) map (_ => ls))
  }
}
