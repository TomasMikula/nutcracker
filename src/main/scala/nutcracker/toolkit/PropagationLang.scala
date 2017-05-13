package nutcracker.toolkit

import nutcracker.util.{ContU, FreeK, Inject}
import nutcracker.{Dom, IDom, OnDemandPropagation, SeqHandler, SeqPreHandler, SeqTrigger, Subscription}
import scalaz.{-\/, IndexedContT, \/, \/-}
import scalaz.syntax.functor._

private[nutcracker] sealed trait PropagationLang[K[_], A]

private[nutcracker] object PropagationLang {

  type TokF[F[_[_], _], A, A0] = (Cell.IncarnationId[FreeK[F, ?], A], Token[A0])
  type TokK[K[_],       A, A0] = (Cell.IncarnationId[K,           A], Token[A0])

  // constructors (the instruction set of a free program)
  case class NewCell[K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, SimpleCellId[K, D]]
  case class Update[K[_], D, U, Δ[_, _]](ref: SimpleCellId[K, D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class Observe[K[_], D, U, Δ[_, _]](ref: SimpleCellId[K, D], f: SeqPreHandler[TokK[K, D, ?], K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[ObserverId]]
  case class Hold[K[_], D](ref: SimpleCellId[K, D], f: (D, Token[D], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class Resume[K[_], D, Δ[_, _], D0 <: D](ref: SimpleCellId[K, D], token: Token[D0], trigger: SeqTrigger[TokK[K, D, ?], K, D, Δ, D0]) extends PropagationLang[K, Unit] {
    type Domain = D
    type Delta[a, b] = Δ[a, b]
    type Arg = D0
  }
  case class RmObserver[K[_], D](ref: SimpleCellId[K, D], oid: ObserverId) extends PropagationLang[K, Unit]

  case class NewAutoCell[K[_], A](setup: (AutoCellId[K, A], CellCycle[A]) => K[Unit]) extends PropagationLang[K, CellId[K, A]]
  case class ObserveAuto[K[_], D, U, Δ[_, _]](ref: AutoCellId[K, D], f: SeqPreHandler[TokK[K, D, ?], K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[(CellCycle[D], ObserverId)]]
  case class HoldAuto[K[_], D](ref: AutoCellId[K, D], f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class ResumeAuto[K[_], D, Δ[_, _], D0 <: D](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[D0], trigger: SeqTrigger[TokK[K, D, ?], K, D, Δ, D0]) extends PropagationLang[K, Unit] {
    type Domain = D
    type Delta[a, b] = Δ[a, b]
    type Arg = D0
  }
  case class RmAutoObserver[K[_], D](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId) extends PropagationLang[K, Unit]
  case class Supply[K[_], D](ref: AutoCellId[K, D], cycle: CellCycle[D], value: D) extends PropagationLang[K, Unit]
  case class AddFinalizer[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A], value: Subscription[K]) extends PropagationLang[K, Option[FinalizerId]]
  case class RemoveFinalizer[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A], id: FinalizerId) extends PropagationLang[K, Unit]
  case class ExclUpdate[K[_], A, U, Δ[_, _]](ref: AutoCellId[K, A], cycle: CellCycle[A], u: U, dom: IDom.Aux[A, U, Δ]) extends PropagationLang[K, Unit]

  case class ExecTriggers[K[_], A](ref: SimpleCellId[K, A]) extends PropagationLang[K, Unit]
  case class ExecTriggersAuto[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def newCell[K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[K, SimpleCellId[K, D]] =
    NewCell[K, D, dom.Update, dom.Delta](d, dom)
  def update[K[_], D, U, Δ[_, _]](ref: SimpleCellId[K, D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Unit] =
    Update[K, D, U, Δ](ref, u, dom)
  def observe[K[_], D, U, Δ[_, _]](ref: SimpleCellId[K, D])(f: SeqPreHandler[TokK[K, D, ?], K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[ObserverId]] =
    Observe[K, D, U, Δ](ref, f, dom)
  def observeAuto[K[_], D, U, Δ[_, _]](ref: AutoCellId[K, D])(f: SeqPreHandler[TokK[K, D, ?], K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[(CellCycle[D], ObserverId)]] =
    ObserveAuto[K, D, U, Δ](ref, f, dom)
  def hold[K[_], D](ref: SimpleCellId[K, D])(f: (D, Token[D], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    Hold[K, D](ref, f)
  def holdAuto[K[_], D](ref: AutoCellId[K, D])(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    HoldAuto[K, D](ref, f)
  def supply[K[_], D](ref: AutoCellId[K, D])(cycle: CellCycle[D], value: D): PropagationLang[K, Unit] =
    Supply(ref, cycle, value)
  def resume[K[_], D, Δ[_, _], D0 <: D](ref: SimpleCellId[K, D], token: Token[D0], trigger: SeqTrigger[TokK[K, D, ?], K, D, Δ, D0]): PropagationLang[K, Unit] =
    Resume[K, D, Δ, D0](ref, token, trigger)
  def resumeAuto[K[_], D, Δ[_, _], D0 <: D](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[D0], trigger: SeqTrigger[TokK[K, D, ?], K, D, Δ, D0]): PropagationLang[K, Unit] =
    ResumeAuto[K, D, Δ, D0](ref, cycle, token, trigger)
  def rmObserver[K[_], D](ref: SimpleCellId[K, D], oid: ObserverId): PropagationLang[K, Unit] =
    RmObserver(ref, oid)
  def rmAutoObserver[K[_], D](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): PropagationLang[K, Unit] =
    RmAutoObserver(ref, cycle, oid)
  def newAutoCell[K[_], A](setup: (AutoCellId[K, A], CellCycle[A]) => K[Unit]): PropagationLang[K, CellId[K, A]] =
    NewAutoCell(setup)
  def addFinalizer[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A], value: Subscription[K]): PropagationLang[K, Option[FinalizerId]] =
    AddFinalizer(ref, cycle, value)
  def removeFinalizer[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A], id: FinalizerId): PropagationLang[K, Unit] =
    RemoveFinalizer(ref, cycle, id)
  def exclUpdate[K[_], A, U, Δ[_, _]](ref: AutoCellId[K, A], cycle: CellCycle[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): PropagationLang[K, Unit] =
    ExclUpdate(ref, cycle, u, dom)
  def execTriggers[K[_], A](ref: SimpleCellId[K, A]): PropagationLang[K, Unit] =
    ExecTriggers(ref)
  def execTriggersAuto[K[_], A](ref: AutoCellId[K, A], cycle: CellCycle[A]): PropagationLang[K, Unit] =
    ExecTriggersAuto(ref, cycle)


  // constructors injected into free programs

  def newCellF[F[_[_], _], D](d: D)(implicit dom: Dom[D], inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, SimpleCellId[FreeK[F, ?], D]] =
    FreeK.liftF(inj(newCell[FreeK[F, ?], D](d)))

  def updateF[F[_[_], _], D, U, Δ[_, _]](ref: SimpleCellId[FreeK[F, ?], D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(update[FreeK[F, ?], D, U, Δ](ref)(u)))

  def observeF[F[_[_], _], D, U, Δ[_, _]](ref: SimpleCellId[FreeK[F, ?], D])(f: SeqPreHandler[TokF[F, D, ?], FreeK[F, ?], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Option[ObserverId]] =
    FreeK.liftF(inj(observe[FreeK[F, ?], D, U, Δ](ref)(f)))

  def observeAutoF[F[_[_], _], D, U, Δ[_, _]](ref: AutoCellId[FreeK[F, ?], D])(f: SeqPreHandler[TokF[F, D, ?], FreeK[F, ?], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Option[(CellCycle[D], ObserverId)]] =
    FreeK.liftF(inj(observeAuto[FreeK[F, ?], D, U, Δ](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], D](ref: SimpleCellId[FreeK[F, ?], D])(f: (D, Token[D], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(hold[FreeK[F, ?], D](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdAutoF[F[_[_], _], D](ref: AutoCellId[FreeK[F, ?], D])(f: (D, CellCycle[D], Token[D], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(holdAuto[FreeK[F, ?], D](ref)(f)))

  def supplyF[F[_[_], _], D](ref: AutoCellId[FreeK[F, ?], D])(cycle: CellCycle[D], value: D)(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(supply[FreeK[F, ?], D](ref)(cycle, value)))

  def resumeF[F[_[_], _], D, Δ[_, _], D0 <: D](ref: SimpleCellId[FreeK[F, ?], D], token: Token[D0], trigger: SeqTrigger[TokF[F, D, ?], FreeK[F, ?], D, Δ, D0])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(resume[FreeK[F, ?], D, Δ, D0](ref, token, trigger)))

  def resumeAutoF[F[_[_], _], D, Δ[_, _], D0 <: D](ref: AutoCellId[FreeK[F, ?], D], cycle: CellCycle[D], token: Token[D0], trigger: SeqTrigger[TokF[F, D, ?], FreeK[F, ?], D, Δ, D0])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(resumeAuto[FreeK[F, ?], D, Δ, D0](ref, cycle, token, trigger)))

  def rmObserverF[F[_[_], _], D](ref: SimpleCellId[FreeK[F, ?], D], oid: ObserverId)(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(rmObserver[FreeK[F, ?], D](ref, oid)))

  def rmAutoObserverF[F[_[_], _], D](ref: AutoCellId[FreeK[F, ?], D], cycle: CellCycle[D], oid: ObserverId)(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(rmAutoObserver[FreeK[F, ?], D](ref, cycle, oid)))

  def newAutoCellF[F[_[_], _], A](setup: (AutoCellId[FreeK[F, ?], A], CellCycle[A]) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, CellId[FreeK[F, ?], A]] =
    FreeK.liftF(inj(newAutoCell[FreeK[F, ?], A](setup)))

  def addFinalizerF[F[_[_], _], A](ref: AutoCellId[FreeK[F, ?], A], cycle: CellCycle[A], value: Subscription[FreeK[F, ?]])(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Option[FinalizerId]] =
    FreeK.liftF(inj(addFinalizer[FreeK[F, ?], A](ref, cycle, value)))

  def removeFinalizerF[F[_[_], _], A](ref: AutoCellId[FreeK[F, ?], A], cycle: CellCycle[A], id: FinalizerId)(implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(removeFinalizer[FreeK[F, ?], A](ref, cycle, id)))

  def exclUpdateF[F[_[_], _], A, U, Δ[_, _]](ref: AutoCellId[FreeK[F, ?], A], cycle: CellCycle[A], u: U)(implicit dom: IDom.Aux[A, U, Δ], inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): FreeK[F, Unit] =
    FreeK.liftF(inj(exclUpdate[FreeK[F, ?], A, U, Δ](ref, cycle, u)))


  implicit def freePropagation[F[_[_], _]](implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): OnDemandPropagation[FreeK[F, ?], SimpleCellId[FreeK[F, ?], ?], CellId[FreeK[F, ?], ?]] =
    new FreePropagation[F]
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit inj: Inject[PropagationLang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]) extends OnDemandPropagation[FreeK[F, ?], SimpleCellId[FreeK[F, ?], ?], CellId[FreeK[F, ?], ?]] {
  import PropagationLang._

  type K[A] = FreeK[F, A]

  type CellCycleId[A] = SimpleCellId[K, A] \/ (AutoCellId[K, A], CellCycle[A])
  type Tok[A, A0] = (CellCycleId[A], Token[A0])

  type Trigger[A, Δ] = SeqTrigger[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A]
  type ExclRef[A] = (AutoCellId[K, A], CellCycle[A])

  override def readOnly[A](ref: SimpleCellId[K, A]): CellId[K, A] = ref

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, SimpleCellId[K, D]] =
    newCellF[F, D](d)

  def newAutoCellC[A](setup: IndexedContT[FreeK[F, ?], Unit, ExclRef[A], A])(implicit dom: Dom[A]): FreeK[F, CellId[K, A]] = {
    val setup1 = (r: AutoCellId[K, A], c: CellCycle[A]) => setup.run(a => supplyF(r)(c, a).as((r, c)))
    newAutoCellF(setup1)
  }

  def addFinalizer[A](ref: ExclRef[A], value: Subscription[FreeK[F, ?]]): FreeK[F, Subscription[FreeK[F, ?]]] =
    addFinalizerF(ref._1, ref._2, value) map {
      case Some(fid) => Subscription(removeFinalizerF(ref._1, ref._2, fid))
      case None      => Subscription()
    }

  def updateImpl[D, U, Δ[_, _]](ref: SimpleCellId[K, D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): FreeK[F, Unit] =
    updateF[F, D, U, Δ](ref)(u)

  def exclUpdateImpl[A, U, Δ[_, _]](ref: ExclRef[A], u: U)(implicit dom: IDom.Aux[A, U, Δ]): FreeK[F, Unit] =
    exclUpdateF[F, A, U, Δ](ref._1, ref._2, u)

  def observeImpl[D, U, Δ](ref: CellId[K, D])(f: D => Trigger[D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[FreeK[F, ?]]] = {
    val h = SeqPreHandler[Tok[D, ?], FreeK[F, ?], D, dom.IDelta](f)
    ref match {
      case ref @ SimpleCellId(_) => observeF[F, D, U, dom.IDelta](ref)(h).map(_.fold(Subscription[FreeK[F, ?]]())(subscription(ref, _)))
      case ref @ AutoCellId(_, _) => observeAutoF[F, D, U, dom.IDelta](ref)(h).map(_.fold(Subscription[FreeK[F, ?]]())({ case (c, oid) => subscription(ref, c, oid) }))
    }
  }

  def observeImplC[A, U, Δ, B](ref: CellId[K, A])(f: A => ContU[FreeK[F, ?], (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[FreeK[F, ?], (Subscription[FreeK[F, ?]], B)] =
    IndexedContT((k: ((Subscription[FreeK[F, ?]], B)) => FreeK[F, Unit]) =>
      ref match {
        case ref @ SimpleCellId(_) =>
          holdF[F, A](ref)((a: A, t: Token[A], oid: ObserverId) =>
            f(a).run({ case (tr, b) => resumeF[F, A, dom.IDelta, A](ref, t, tr) >> k((subscription(ref, oid), b)) })
          ).void
        case ref @ AutoCellId(_, _) =>
          holdAutoF[F, A](ref)((a: A, cycle: CellCycle[A], t: Token[A], oid: ObserverId) =>
            f(a).run({ case (tr, b) => resumeAutoF[F, A, dom.IDelta, A](ref, cycle, t, tr) >> k((subscription(ref, cycle, oid), b)) })
          ).void
      })

  private def subscription[D](ref: SimpleCellId[K, D], oid: ObserverId): Subscription[FreeK[F, ?]] =
    Subscription(rmObserverF(ref, oid))

  private def subscription[D](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): Subscription[FreeK[F, ?]] =
    Subscription(rmAutoObserverF(ref, cycle, oid))

  override def discard[A, Δ]: Trigger[A, Δ] =
    SeqTrigger.Discard[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A]()
  override def fire[A, Δ](action: FreeK[F, Unit]): Trigger[A, Δ] =
    SeqTrigger.Fire[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A](action)
  override def sleep[A, Δ](next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    SeqTrigger.Sleep[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A](SeqHandler[Tok[A, ?], K, A, Δ](next))
  override def fireReload[A, Δ](action: FreeK[F, Unit], next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    SeqTrigger.FireReload[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A](action, SeqHandler[Tok[A, ?], K, A, Δ](next))
  override def reconsider[A, Δ](action: FreeK[F, Trigger[A, Δ]]): Trigger[A, Δ] =
    SeqTrigger.Reconsider[Tok[A, ?], FreeK[F, ?], A, λ[(α, β) => Δ], A](tok => tok._1 match {
      case -\/(r)      => action >>= (tr => resumeF    [F, A, λ[(α, β) => Δ], A](r,    tok._2, tr))
      case \/-((r, c)) => action >>= (tr => resumeAutoF[F, A, λ[(α, β) => Δ], A](r, c, tok._2, tr))
    })
}