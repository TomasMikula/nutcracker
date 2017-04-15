package nutcracker.rel

import nutcracker.util.Choose
import scalaz.Bind
import scalaz.syntax.bind._
import shapeless.HList

abstract class Recipe[L <: HList, C <: HList, M[_]](val choose: Choose[L, C]) {
  def create(c: C): M[L]

  def andThen(f: L => M[Unit])(implicit M: Bind[M]): Recipe[L, C, M] = new Recipe[L, C, M](choose) {
    def create(c: C): M[L] = Recipe.this.create(c) >>! f
  }
}
