package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.Id._
import scalaz.{Functor, StreamT}

object PropBranchProm extends Language {
  type Stream[A] = StreamT[Id, A]
  type BranchL[K[_], A] = BranchLang[Stream, K, A]
  type BranchS[K[_]] = BranchStore[Stream, K]

  type Lang0[K[_], A] = CoproductK[BranchL, PromiseLang, K, A]
  type Lang1[K[_], A] = CoproductK[PropagationLang, Lang0, K, A]
  type Vocabulary[K[_], A] = CoyonedaK[Lang1, K, A]

  type State0[K[_]] = ProductK[BranchS, PromiseStore, K]
  type State[K[_]] = ProductK[PropagationStore, State0, K]

  type Dirty0[K[_]] = ProductK[AlwaysClean, AlwaysClean, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, Dirty0, K]

  def emptyState[K[_]]: State[K] =  {
    import ProductK._

    val emptyD = PropagationStore.empty[K]
    val emptyB: BranchS[K] = BranchStore.empty[Stream, K]
    val emptyP = PromiseStore.empty[K]

    emptyD :*: emptyB :*: emptyP
  }

  val interpreter: Interpreter[Vocabulary, State, Dirty] = implicitly[Interpreter[Vocabulary, State, Dirty]]

  def vocabularyFunctor[K[_]]: Functor[Vocabulary[K, ?]] = CoyonedaK.functorInstance[Lang1, K] // could not find it implicitly
  def dirtyMonoidK: MonoidK[Dirty] = implicitly[MonoidK[Dirty]]
}
