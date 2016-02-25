package nutcracker

import monocle.Lens
import nutcracker.Assessment._
import nutcracker.PropagationLang._
import nutcracker.rel.{RelDB, RelLang}

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.{Applicative, Monoid, ~>}
import scalaz.syntax.applicative._

final class PropRelCost[C: Monoid] extends Language {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = ConstK[C, K]

  type Vocabulary0[K[_], A] = CoproductK[RelLang, CostL, K, A]
  type Vocabulary[K[_], A] = CoproductK[PropagationLang, Vocabulary0, K, A]

  type State0[K[_]] = ProductK[RelDB, CostS, K]
  type State[K[_]] = ProductK[PropagationStore, State0, K]

  type Dirty0[K[_]] = ProductK[AlwaysClean, AlwaysClean, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, Dirty0, K]

  val interpreter: Interpreter.Aux[Vocabulary, State, Dirty] = implicitly[Interpreter.Aux[Vocabulary, State, Dirty]]
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]


  def naiveAssess[K[_]: Applicative](s: State[K])(implicit tr: FreeK[PropagationLang, ?] ~> K): Assessment[List[(State[K], K[Unit])]] = {
    if(propStore.get(s).failedVars.nonEmpty) Failed
    else if(propStore.get(s).unresolvedVars.isEmpty) Done
    else {
      def splitDomain[A, D](ref: DomRef[A, D]): Option[List[K[Unit]]] = {
        val (d, domain) = propStore.get(s).getDomain(ref)
        domain.values(d) match {
          case Domain.Empty() => Some(Nil)
          case Domain.Just(a) => Some(().pure[K] :: Nil)
          case Domain.Many(branchings) =>
            if(branchings.isEmpty) None
            else Some(branchings.head map { d => tr(intersectF(ref)(d)) })
        }
      }

      propStore.get(s).unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches.map { k => (s, k) })
      }).getOrElse(Stuck)
    }
  }
}
