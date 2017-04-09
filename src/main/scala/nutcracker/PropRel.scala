package nutcracker

import nutcracker.rel.{RelModule, RelToolkit, Relations}
import nutcracker.util.FreeKT
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import scalaz.{Lens, Monad}
import scalaz.Id.Id

trait PropRelToolkit extends PropagationToolkit with RelToolkit

object PropRelToolkit {
  val instance: PropRelToolkit = PropRel
}

object PropRel extends PropagationBundle with PropRelToolkit {
  val Prop = Propagation.module
  val RelMod = RelModule.instance

  type Var[a] = Prop.Var[a]
  type Val[a] = Prop.Val[a]

  type Lang[K[_], A] = (Prop.Lang  :++: RelMod.Lang )#Out[K, A]
  type State[K[_]]   = (Prop.State :**: RelMod.State)#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refOrder = Prop.refOrder
  implicit def refShow = Prop.refShow
  implicit def prgMonad: Monad[Prg] = FreeKT.freeKTMonad[Lang, Id]

  implicit val propagationApi: Propagation[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val relationsApi: Relations[Prg] =
    RelMod.freeRelations[Lang]

  def empty[K[_]]: State[K] =
    Prop.empty[K] :*: RelMod.empty[K]

  def fetch[K[_], A](ref: Val[A], s: State[K]) =
    Prop.fetch(ref, s._1)

  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) =
    interpreter(p).run(s)

  val interpreter = (Prop.interpreter :&&: RelMod.interpreter).freeInstance
  def propStore: Lens[State[Prg], Prop.State[Prg]] = implicitly[Lens[State[Prg], Prop.State[Prg]]]
}
