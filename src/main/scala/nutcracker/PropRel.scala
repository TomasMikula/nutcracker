package nutcracker

import scala.language.higherKinds
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import scalaz.Lens
import scalaz.Id._
import scalaz.~>

object PropRel {
  val Prop = Propagation.module
  import Prop._

  type Ref[a] = Prop.Ref[a]

  type Vocabulary[K[_], A] = (Prop.Lang  :++: RelLang)#Out[K, A]
  type State[K[_]]         = (Prop.State :**: RelDB  )#Out[K]

  val interpreter = (Prop.interpreter :&&: RelDB.interpreter).freeInstance
  def propStore: Lens[State[Q], Prop.State[Q]] = implicitly[Lens[State[Q], Prop.State[Q]]]
  def emptyState: State[Q] = Prop.emptyF[Vocabulary] :**: RelDB.empty[Q]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] =
    Prop.naiveAssess(propStore)
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q] => ?)](pa => s => Prop.fetchResult(propStore.get(s))(pa).get)
}
