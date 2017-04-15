package nutcracker

import nutcracker.CellCycle.LiveCycle
import nutcracker.util.{ContU, FreeK, FreeKT, InjectK}
import scalaz.{Functor, IndexedContT}
import scalaz.Id._
import scalaz.syntax.monad._
import shapeless.HList

private[nutcracker] sealed trait PropagationLang[Ref[_], K[_], A]

private[nutcracker] object PropagationLang {

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Ref[D]]
  case class Update[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit]
  case class Observe[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Token, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Option[ObserverId]]
  case class Hold[Ref[_], K[_], D](ref: Ref[D], f: (D, Token[D], ObserverId) => K[Unit]) extends PropagationLang[Ref, K, ObserverId]
  case class Supply[Ref[_], K[_], D](ref: Ref[D], cycle: LiveCycle[D], value: D) extends PropagationLang[Ref, K, Unit]
  case class Resume[Ref[_], K[_], D, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0]) extends PropagationLang[Ref, K, Unit] {
    type Domain = D
    type Delta[a, b] = Δ[a, b]
    type Arg = D0
  }
  case class RmObserver[Ref[_], K[_], D](ref: Ref[D], oid: ObserverId) extends PropagationLang[Ref, K, Unit]
  case class SelTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L], f: L => (Option[K[Unit]], Boolean)) extends PropagationLang[Ref, K, Unit]

  case class NewAutoCell[Ref[_], K[_], A](setup: IndexedContT[K, Unit, (Ref[A], LiveCycle[A]), A], supply: (Ref[A], LiveCycle[A], A) => K[Unit], dom: Dom[A], ftor: Functor[K]) extends PropagationLang[Ref, K, Ref[A]]
  case class AddFinalizer[Ref[_], K[_], A](ref: Ref[A], cycle: LiveCycle[A], value: Subscription[K]) extends PropagationLang[Ref, K, Option[FinalizerId]]
  case class RemoveFinalizer[Ref[_], K[_], A](ref: Ref[A], cycle: LiveCycle[A], id: FinalizerId) extends PropagationLang[Ref, K, Unit]
  case class ExclUpdate[Ref[_], K[_], A, U, Δ[_, _]](ref: Ref[A], cycle: LiveCycle[A], u: U, dom: IDom.Aux[A, U, Δ]) extends PropagationLang[Ref, K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, K, Ref[D]] =
    NewCell[Ref, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Update[Ref, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Option[ObserverId]] =
    Observe[Ref, K, D, U, Δ](ref, f, dom)
  def hold[Ref[_], K[_], D](ref: Ref[D])(f: (D, Token[D], ObserverId) => K[Unit]): PropagationLang[Ref, K, ObserverId] =
    Hold[Ref, K, D](ref, f)
  def supply[Ref[_], K[_], D](ref: Ref[D])(cycle: LiveCycle[D], value: D): PropagationLang[Ref, K, Unit] =
    Supply(ref, cycle, value)
  def resume[Ref[_], K[_], D, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0]): PropagationLang[Ref, K, Unit] =
    Resume[Ref, K, D, Δ, D0](ref, token, trigger)
  def rmObserver[Ref[_], K[_], D](ref: Ref[D], oid: ObserverId): PropagationLang[Ref, K, Unit] =
    RmObserver(ref, oid)
  def selTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L])(f: L => (Option[K[Unit]], Boolean)): PropagationLang[Ref, K, Unit] =
    SelTrigger(sel, f)
  def newAutoCell[Ref[_], K[_], A](setup: IndexedContT[K, Unit, (Ref[A], LiveCycle[A]), A], supply: (Ref[A], LiveCycle[A], A) => K[Unit])(implicit dom: Dom[A], K: Functor[K]): PropagationLang[Ref, K, Ref[A]] =
    NewAutoCell(setup, supply, dom, K)
  def addFinalizer[Ref[_], K[_], A](ref: Ref[A], cycle: LiveCycle[A], value: Subscription[K]): PropagationLang[Ref, K, Option[FinalizerId]] =
    AddFinalizer(ref, cycle, value)
  def removeFinalizer[Ref[_], K[_], A](ref: Ref[A], cycle: LiveCycle[A], id: FinalizerId): PropagationLang[Ref, K, Unit] =
    RemoveFinalizer(ref, cycle, id)
  def exclUpdate[Ref[_], K[_], A, U, Δ[_, _]](ref: Ref[A], cycle: LiveCycle[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): PropagationLang[Ref, K, Unit] =
    ExclUpdate(ref, cycle, u, dom)


  // constructors injected into free programs

  def newCellF[F[_[_], _], Ref[_], D](d: D)(implicit dom: Dom[D], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(newCell[Ref, FreeK[F, ?], D](d))

  def updateF[F[_[_], _], Ref[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(update[Ref, FreeK[F, ?], D, U, Δ](ref)(u))

  def observeF[F[_[_], _], Ref[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Token, FreeK[F, Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Option[ObserverId]] =
    FreeK.injLiftF(observe[Ref, FreeK[F, ?], D, U, Δ](ref)(f))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], Ref[_], D](ref: Ref[D])(f: (D, Token[D], ObserverId) => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, ObserverId] =
    FreeK.injLiftF(hold[Ref, FreeK[F, ?], D](ref)(f))

  def supplyF[F[_[_], _], Ref[_], D](ref: Ref[D])(cycle: LiveCycle[D], value: D)(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(supply[Ref, FreeK[F, ?], D](ref)(cycle, value))

  def resumeF[F[_[_], _], Ref[_], D, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, FreeK[F, Unit], D, Δ, D0])(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, FreeK[F, ?], D, Δ, D0](ref, token, trigger))

  def rmObserverF[F[_[_], _], Ref[_], D](ref: Ref[D], oid: ObserverId)(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(rmObserver[Ref, FreeK[F, ?], D](ref, oid))

  def newAutoCellF[F[_[_], _], Ref[_], A](setup: IndexedContT[FreeK[F, ?], Unit, (Ref[A], LiveCycle[A]), A])(supply: (Ref[A], LiveCycle[A], A) => FreeK[F, Unit])(implicit dom: Dom[A], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Ref[A]] =
    FreeK.injLiftF(newAutoCell[Ref, FreeK[F, ?], A](setup, supply)(dom, FreeKT.freeKTMonad[F, Id]))

  def addFinalizerF[F[_[_], _], Ref[_], A](ref: Ref[A], cycle: LiveCycle[A], value: Subscription[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Option[FinalizerId]] =
    FreeK.injLiftF(addFinalizer[Ref, FreeK[F, ?], A](ref, cycle, value))

  def removeFinalizerF[F[_[_], _], Ref[_], A](ref: Ref[A], cycle: LiveCycle[A], id: FinalizerId)(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(removeFinalizer[Ref, FreeK[F, ?], A](ref, cycle, id))

  def exclUpdateF[F[_[_], _], Ref[_], A, U, Δ[_, _]](ref: Ref[A], cycle: LiveCycle[A], u: U)(implicit dom: IDom.Aux[A, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(exclUpdate[Ref, FreeK[F, ?], A, U, Δ](ref, cycle, u))


  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang[CellId, ?[_], ?], F]): OnDemandPropagation[FreeK[F, ?], CellId, CellId] =
    new FreePropagation[F]
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang[CellId, ?[_], ?], F]) extends OnDemandPropagation[FreeK[F, ?], CellId, CellId] {
  import PropagationLang._

  type Trigger[A, Δ] = SeqTrigger[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A]
  type ExclRef[A] = (CellId[A], LiveCycle[A])

  override def readOnly[A](ref: CellId[A]): CellId[A] = ref

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, CellId[D]] =
    newCellF[F, CellId, D](d)

  def newAutoCell[A](setup: IndexedContT[FreeK[F, ?], Unit, ExclRef[A], A])(implicit dom: Dom[A]): FreeK[F, CellId[A]] =
    newAutoCellF(setup)(supplyF(_)(_, _)(inj))

  def addFinalizer[A](ref: ExclRef[A], value: Subscription[FreeK[F, ?]]): FreeK[F, Subscription[FreeK[F, ?]]] =
    addFinalizerF(ref._1, ref._2, value) map {
      case Some(fid) => Subscription(removeFinalizerF(ref._1, ref._2, fid))
      case None      => Subscription()
    }

  def updateImpl[D, U, Δ[_, _]](ref: CellId[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): FreeK[F, Unit] =
    updateF[F, CellId, D, U, Δ](ref)(u)

  def exclUpdateImpl[A, U, Δ[_, _]](ref: ExclRef[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): FreeK[F, Unit] =
    exclUpdateF[F, CellId, A, U, Δ](ref._1, ref._2, u)

  def observeImpl[D, U, Δ](ref: CellId[D])(f: D => Trigger[D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[FreeK[F, ?]]] = {
    observeF[F, CellId, D, U, dom.IDelta](ref)(SeqPreHandler[Token, FreeK[F, Unit], D, dom.IDelta](f))
      .map(_.fold(Subscription[FreeK[F, ?]]())(subscription(ref, _)))
  }

  def observeImplC[A, U, Δ, B](src: CellId[A])(f: A => ContU[FreeK[F, ?], (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[FreeK[F, ?], (Subscription[FreeK[F, ?]], B)] = {
    IndexedContT((k: ((Subscription[FreeK[F, ?]], B)) => FreeK[F, Unit]) =>
      holdF[F, CellId, A](src)((a: A, t: Token[A], oid: ObserverId) =>
        f(a).run({ case (tr, b) => resumeF[F, CellId, A, dom.IDelta, A](src, t, tr) >> k((subscription(src, oid), b)) })
      ).void)
  }

  def selTrigger[L <: HList](sel: Sel[CellId, L])(f: L => (Option[FreeK[F, Unit]], Boolean)): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.selTrigger[CellId, FreeK[F, ?], L](sel)(f))

  private def subscription[D](ref: CellId[D], oid: ObserverId): Subscription[FreeK[F, ?]] =
    Subscription(rmObserverF(ref, oid))

  override def discard[A, Δ]: Trigger[A, Δ] =
    SeqTrigger.Discard[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A]()
  override def fire[A, Δ](action: FreeK[F, Unit]): Trigger[A, Δ] =
    SeqTrigger.Fire[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A](action)
  override def sleep[A, Δ](next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    SeqTrigger.Sleep[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A](SeqHandler(next))
  override def fireReload[A, Δ](action: FreeK[F, Unit], next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    SeqTrigger.FireReload[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A](action, SeqHandler(next))
  override def reconsider[A, Δ](action: FreeK[F, Trigger[A, Δ]]): Trigger[A, Δ] =
    SeqTrigger.Reconsider[Token, FreeK[F, Unit], A, λ[(α, β) => Δ], A]((ref: Val[A], tok: Token[A]) => action >>= (tr => resumeF[F, CellId, A, λ[(α, β) => Δ], A](ref, tok, tr)))
}