package nutcracker

import scala.language.higherKinds
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import scalaz.Lens
import scalaz.~>

object PropRel extends PropagationBundle {
  val Prop = Propagation.module

  type Ref[a] = Prop.Ref[a]

  type Lang[K[_], A] = (Prop.Lang  :++: RelLang)#Out[K, A]
  type State[K[_]]   = (Prop.State :**: RelDB  )#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refShow = Prop.refShow

  implicit val propagationApi: Propagation[Prg, Ref] =
    Prop.freePropagation[Lang]

  def empty[K[_]]: State[K] =
    Prop.empty[K] :*: RelDB.empty[K]

  def fetch[K[_], A](ref: Ref[A], s: State[K]) =
    Prop.fetch(ref, s._1)

  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) =
    interpreter(p).run(s)

  val interpreter = (Prop.interpreter :&&: RelDB.interpreter).freeInstance
  def propStore: Lens[State[Prg], Prop.State[Prg]] = implicitly[Lens[State[Prg], Prop.State[Prg]]]

  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore.get(s))(pa).get)
}
