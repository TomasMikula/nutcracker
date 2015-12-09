package nutcracker

import monocle.Lens

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.Id._
import scalaz.{Monoid, Functor, StreamT}

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

  def vocabularyFunctor[K[_]]: Functor[Vocabulary[K, ?]] = CoyonedaK.functorInstance[Lang0, K] // could not find it implicitly
  def dirtyMonoidK: MonoidK[Dirty] = implicitly[MonoidK[Dirty]]
}
