package nutcracker

import monocle.Lens
import nutcracker.Assessment._
import nutcracker.PropagationLang._

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.Id._
import scalaz.{Applicative, Monoid, Functor, StreamT, ~>}
import scalaz.syntax.applicative._

final class PropBranchPromCost[C: Monoid] extends Language {
  type Stream[A] = StreamT[Id, A]
  type BranchL[K[_], A] = BranchLang[Stream, K, A]
  type BranchS[K[_]] = BranchStore[Stream, K]
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = ConstK[C, K]

  type Lang2[K[_], A] = CoproductK[PromiseLang, CostL, K, A]
  type Lang1[K[_], A] = CoproductK[BranchL, Lang2, K, A]
  type Lang0[K[_], A] = CoproductK[PropagationLang, Lang1, K, A]
  type Vocabulary[K[_], A] = CoyonedaK[Lang0, K, A]

  type State1[K[_]] = ProductK[PromiseStore, CostS, K]
  type State0[K[_]] = ProductK[BranchS, State1, K]
  type State[K[_]] = ProductK[PropagationStore, State0, K]

  type Dirty1[K[_]] = ProductK[AlwaysClean, AlwaysClean, K]
  type Dirty0[K[_]] = ProductK[AlwaysClean, Dirty1, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, Dirty0, K]

  def emptyState[K[_]]: State[K] =  {
    import ProductK._

    val emptyD = PropagationStore.empty[K]
    val emptyB: BranchS[K] = BranchStore.empty[Stream, K]
    val emptyP = PromiseStore.empty[K]
    val zeroC: CostS[K] = implicitly[Monoid[C]].zero

    emptyD :*: emptyB :*: emptyP :*: zeroC
  }

  val interpreter: Interpreter[Vocabulary, State, Dirty] = implicitly[Interpreter[Vocabulary, State, Dirty]]
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def branchStore[K[_]]: Lens[State[K], BranchS[K]] = implicitly[Lens[State[K], BranchS[K]]]
  def promStore[K[_]]: Lens[State[K], PromiseStore[K]] = implicitly[Lens[State[K], PromiseStore[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  def vocabularyFunctor[K[_]]: Functor[Vocabulary[K, ?]] = CoyonedaK.functorInstance[Lang0, K] // could not find it implicitly
  def dirtyMonoidK: MonoidK[Dirty] = implicitly[MonoidK[Dirty]]


  def naiveAssess[K[_]: Applicative](s: State[K])(implicit tr: FreeK[PropagationLang, ?] ~> K): Assessment[StreamT[Id, (State[K], K[Unit])]] = {
    if(propStore.get(s).failedVars.nonEmpty) Failed
    else branchStore.get(s).branches match {
      case b::bs =>
        val s1 = branchStore.set(new BranchStore[StreamT[Id, ?], K](bs))(s)
        Incomplete(b map { k => (s1, k) })
      case Nil =>
        if(propStore.get(s).unresolvedVars.isEmpty) Done
        else {

          def splitDomain[A, D](ref: DomRef[A, D]): StreamT[Id, K[Unit]] = {
            val (d, domain) = propStore.get(s).getDomain(ref)
            domain.values(d) match {
              case Domain.Empty() => StreamT.empty
              case Domain.Just(a) => ().pure[K] :: StreamT.empty[Id, K[Unit]]
              case Domain.Many(branchings) => StreamT.fromIterable(branchings.head) map { d =>
                tr(intersectF(ref)(d))
              }
            }
          }

          Incomplete(splitDomain(propStore.get(s).unresolvedVars.head) map { k => (s, k) })
        }
    }
  }
}