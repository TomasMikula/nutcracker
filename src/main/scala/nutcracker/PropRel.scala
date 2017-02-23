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

  type Ref[a] = Prop.Ref[a]

  type Vocabulary[K[_], A] = (Prop.Lang  :++: RelLang)#Out[K, A]
  type State[K[_]]         = (Prop.State :**: RelDB  )#Out[K]

  type Prg[A] = FreeK[Vocabulary, A]

  implicit val propagation: Propagation[Prg, Ref] =
    Prop.freePropagation[Vocabulary]

  val interpreter = (Prop.interpreter :&&: RelDB.interpreter).freeInstance
  def propStore: Lens[State[Prg], Prop.State[Prg]] = implicitly[Lens[State[Prg], Prop.State[Prg]]]
  def emptyState: State[Prg] = Prop.empty[Prg] :*: RelDB.empty[Prg]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] =
    Prop.naiveAssess(propStore)
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore.get(s))(pa).get)
}
