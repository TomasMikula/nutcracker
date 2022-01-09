package nutcracker.toolkit

import nutcracker.util.{ContU, Forall, FreeK, Id, IndexedContT, Inject}
import nutcracker.{Dom, IDom, OnDemandPropagation, SeqHandler, SeqPreHandler, SeqTrigger, Subscription}
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.functor._

private[nutcracker] sealed trait PropagationLang[K[_], A]

private[nutcracker] object PropagationLang {

  type TokF[F[_[_], _], D[_], I] = (Cell.IncarnationId[FreeK[F, *], D], Token[I])
  type TokK[K[_],       D[_], I] = (Cell.IncarnationId[K,           D], Token[I])

  // constructors (the instruction set of a free program)
  case class NewCell[K[_], D[_], U[_], Δ[_, _], I](d: D[I], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, SimpleCellId[K, D]]
  case class Update[K[_], D[_], U[_], Δ[_, _], J](ref: SimpleCellId[K, D], u: U[J], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class Observe[K[_], D[_], U[_], Δ[_, _]](ref: SimpleCellId[K, D], f: SeqPreHandler[TokK[K, D, *], K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[ObserverId]]
  case class Hold[K[_], D[_]](ref: SimpleCellId[K, D], f: [i] => (D[i], Token[i], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class Resume[K[_], D[_], Δ[_, _], I](ref: SimpleCellId[K, D], token: Token[I], trigger: SeqTrigger[TokK[K, D, *], K, D, Δ, I]) extends PropagationLang[K, Unit] {
    type Domain[i] = D[i]
    type Delta[i, j] = Δ[i, j]
    type Idx = I
  }
  case class RmObserver[K[_], D[_]](ref: SimpleCellId[K, D], oid: ObserverId) extends PropagationLang[K, Unit]

  case class NewAutoCell[K[_], D[_]](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]) extends PropagationLang[K, CellId[K, D]]
  case class ObserveAuto[K[_], D[_], U[_], Δ[_, _]](ref: AutoCellId[K, D], f: SeqPreHandler[TokK[K, D, *], K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[(CellCycle[D], ObserverId)]]
  case class HoldAuto[K[_], D[_]](ref: AutoCellId[K, D], f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class ResumeAuto[K[_], D[_], Δ[_, _], I](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[I], trigger: SeqTrigger[TokK[K, D, *], K, D, Δ, I]) extends PropagationLang[K, Unit] {
    type Domain[i] = D[i]
    type Delta[i, j] = Δ[i, j]
    type Idx = I
  }
  case class RmAutoObserver[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId) extends PropagationLang[K, Unit]
  case class Supply[K[_], D[_], I](ref: AutoCellId[K, D], cycle: CellCycle[D], value: D[I]) extends PropagationLang[K, Unit]
  case class AddFinalizer[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], value: Subscription[K]) extends PropagationLang[K, Option[FinalizerId]]
  case class RemoveFinalizer[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], id: FinalizerId) extends PropagationLang[K, Unit]
  case class ExclUpdate[K[_], D[_], U[_], Δ[_, _], J](ref: AutoCellId[K, D], cycle: CellCycle[D], u: U[J], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]

  case class ExecTriggers[K[_], D[_]](ref: SimpleCellId[K, D]) extends PropagationLang[K, Unit]
  case class ExecTriggersAuto[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def newCell[K[_], D[_], I](d: D[I])(implicit dom: IDom[D]): PropagationLang[K, SimpleCellId[K, D]] =
    NewCell[K, D, dom.IUpdate, dom.IDelta, I](d, dom)
  def update[K[_], D[_], U[_], Δ[_, _], J](ref: SimpleCellId[K, D])(u: U[J])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Unit] =
    Update[K, D, U, Δ, J](ref, u, dom)
  def observe[K[_], D[_], U[_], Δ[_, _]](ref: SimpleCellId[K, D])(f: SeqPreHandler[TokK[K, D, *], K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[ObserverId]] =
    Observe[K, D, U, Δ](ref, f, dom)
  def observeAuto[K[_], D[_], U[_], Δ[_, _]](ref: AutoCellId[K, D])(f: SeqPreHandler[TokK[K, D, *], K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[(CellCycle[D], ObserverId)]] =
    ObserveAuto[K, D, U, Δ](ref, f, dom)
  def hold[K[_], D[_]](ref: SimpleCellId[K, D])(f: [i] => (D[i], Token[i], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    Hold[K, D](ref, f)
  def holdAuto[K[_], D[_]](ref: AutoCellId[K, D])(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    HoldAuto[K, D](ref, f)
  def supply[K[_], D[_], I](ref: AutoCellId[K, D])(cycle: CellCycle[D], value: D[I]): PropagationLang[K, Unit] =
    Supply(ref, cycle, value)
  def resume[K[_], D[_], Δ[_, _], I0](ref: SimpleCellId[K, D], token: Token[I0], trigger: SeqTrigger[TokK[K, D, *], K, D, Δ, I0]): PropagationLang[K, Unit] =
    Resume[K, D, Δ, I0](ref, token, trigger)
  def resumeAuto[K[_], D[_], Δ[_, _], I0](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[I0], trigger: SeqTrigger[TokK[K, D, *], K, D, Δ, I0]): PropagationLang[K, Unit] =
    ResumeAuto[K, D, Δ, I0](ref, cycle, token, trigger)
  def rmObserver[K[_], D[_]](ref: SimpleCellId[K, D], oid: ObserverId): PropagationLang[K, Unit] =
    RmObserver(ref, oid)
  def rmAutoObserver[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): PropagationLang[K, Unit] =
    RmAutoObserver(ref, cycle, oid)
  def newAutoCell[K[_], D[_]](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]): PropagationLang[K, CellId[K, D]] =
    NewAutoCell(setup)
  def addFinalizer[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], value: Subscription[K]): PropagationLang[K, Option[FinalizerId]] =
    AddFinalizer(ref, cycle, value)
  def removeFinalizer[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], id: FinalizerId): PropagationLang[K, Unit] =
    RemoveFinalizer(ref, cycle, id)
  def exclUpdate[K[_], D[_], U[_], Δ[_, _], J](ref: AutoCellId[K, D], cycle: CellCycle[D], u: U[J])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Unit] =
    ExclUpdate(ref, cycle, u, dom)
  def execTriggers[K[_], D[_]](ref: SimpleCellId[K, D]): PropagationLang[K, Unit] =
    ExecTriggers(ref)
  def execTriggersAuto[K[_], D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D]): PropagationLang[K, Unit] =
    ExecTriggersAuto(ref, cycle)


  // constructors injected into free programs

  def newCellF[F[_[_], _], D[_], I](d: D[I])(implicit dom: IDom[D], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, SimpleCellId[FreeK[F, *], D]] =
    FreeK.liftF(inj(newCell[FreeK[F, *], D, I](d)))

  def updateF[F[_[_], _], D[_], U[_], Δ[_, _], J](ref: SimpleCellId[FreeK[F, *], D])(u: U[J])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(update[FreeK[F, *], D, U, Δ, J](ref)(u)))

  def observeF[F[_[_], _], D[_], U[_], Δ[_, _]](ref: SimpleCellId[FreeK[F, *], D])(f: SeqPreHandler[TokF[F, D, *], FreeK[F, *], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Option[ObserverId]] =
    FreeK.liftF(inj(observe[FreeK[F, *], D, U, Δ](ref)(f)))

  def observeAutoF[F[_[_], _], D[_], U[_], Δ[_, _]](ref: AutoCellId[FreeK[F, *], D])(f: SeqPreHandler[TokF[F, D, *], FreeK[F, *], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Option[(CellCycle[D], ObserverId)]] =
    FreeK.liftF(inj(observeAuto[FreeK[F, *], D, U, Δ](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], D[_]](ref: SimpleCellId[FreeK[F, *], D])(f: [i] => (D[i], Token[i], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(hold[FreeK[F, *], D](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdAutoF[F[_[_], _], D[_]](ref: AutoCellId[FreeK[F, *], D])(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(holdAuto[FreeK[F, *], D](ref)(f)))

  def supplyF[F[_[_], _], D[_], I](ref: AutoCellId[FreeK[F, *], D])(cycle: CellCycle[D], value: D[I])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(supply[FreeK[F, *], D, I](ref)(cycle, value)))

  def resumeF[F[_[_], _], D[_], Δ[_, _], I0](ref: SimpleCellId[FreeK[F, *], D], token: Token[I0], trigger: SeqTrigger[TokF[F, D, *], FreeK[F, *], D, Δ, I0])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(resume[FreeK[F, *], D, Δ, I0](ref, token, trigger)))

  def resumeAutoF[F[_[_], _], D[_], Δ[_, _], I0](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], token: Token[I0], trigger: SeqTrigger[TokF[F, D, *], FreeK[F, *], D, Δ, I0])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(resumeAuto[FreeK[F, *], D, Δ, I0](ref, cycle, token, trigger)))

  def rmObserverF[F[_[_], _], D[_]](ref: SimpleCellId[FreeK[F, *], D], oid: ObserverId)(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(rmObserver[FreeK[F, *], D](ref, oid)))

  def rmAutoObserverF[F[_[_], _], D[_]](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], oid: ObserverId)(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(rmAutoObserver[FreeK[F, *], D](ref, cycle, oid)))

  def newAutoCellF[F[_[_], _], D[_]](setup: (AutoCellId[FreeK[F, *], D], CellCycle[D]) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, CellId[FreeK[F, *], D]] =
    FreeK.liftF(inj(newAutoCell[FreeK[F, *], D](setup)))

  def addFinalizerF[F[_[_], _], D[_]](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], value: Subscription[FreeK[F, *]])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Option[FinalizerId]] =
    FreeK.liftF(inj(addFinalizer[FreeK[F, *], D](ref, cycle, value)))

  def removeFinalizerF[F[_[_], _], D[_]](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], id: FinalizerId)(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(removeFinalizer[FreeK[F, *], D](ref, cycle, id)))

  def exclUpdateF[F[_[_], _], D[_], U[_], Δ[_, _], J](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], u: U[J])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(exclUpdate[FreeK[F, *], D, U, Δ, J](ref, cycle, u)))


  implicit def freePropagation[F[_[_], _]](implicit
    inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]],
  ): OnDemandPropagation.Aux[FreeK[F, *], [a] =>> SimpleCellId[FreeK[F, *], [i] =>> a], [a] =>> CellId[FreeK[F, *], [i] =>> a]] =
    new FreePropagation[F]
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit
  inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]],
) extends OnDemandPropagation[FreeK[F, *]] {
  import PropagationLang._

  type K[A] = FreeK[F, A]

  override type Var[A] = SimpleCellId[K, [i] =>> A]
  override type Val[A] = CellId[K, [i] =>> A]

  type CellCycleId[A] = Var[A] \/ (AutoCellId[K, [i] =>> A], CellCycle[[i] =>> A])
  type Tok[A, I] = (CellCycleId[A], Token[I])

  type TriggerI[A, Δ, I] = SeqTrigger[Tok[A, *], K, [i] =>> A, [i, j] =>> Δ, I]
  type Trigger[A, Δ] = Forall[TriggerI[A, Δ, *]]
  type ExclRef[A] = (AutoCellId[K, [i] =>> A], CellCycle[[i] =>> A])

  override def readOnly[A](ref: Var[A]): Val[A] = ref

  override def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Var[D]] =
    newCellF[F, [i] =>> D, Any](d)

  override def newAutoCellC[A](setup: IndexedContT[Unit, ExclRef[A], K, A])(implicit dom: Dom[A]): FreeK[F, Val[A]] = {
    val setup1 = (r: AutoCellId[K, [i] =>> A], c: CellCycle[[i] =>> A]) => setup.run(Id(a => supplyF(r)(c, a).as((r, c))))
    newAutoCellF(setup1)
  }

  override def addFinalizer[A](ref: ExclRef[A], value: Subscription[K]): FreeK[F, Subscription[K]] =
    addFinalizerF(ref._1, ref._2, value) map {
      case Some(fid) => Subscription(removeFinalizerF(ref._1, ref._2, fid))
      case None      => Subscription()
    }

  override def updateImpl[D, U, Δ](ref: Var[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    updateF[F, [i] =>> D, [i] =>> U, [i, j] =>> Δ, Any](ref)(u)

  override def exclUpdateImpl[A, U, Δ](ref: ExclRef[A], u: U)(implicit dom: Dom.Aux[A, U, Δ]): FreeK[F, Unit] =
    exclUpdateF[F, [i] =>> A, [i] =>> U, [i, j] =>> Δ, Any](ref._1, ref._2, u)

  override def observeImpl[D, U, Δ](ref: Val[D])(f: D => Trigger[D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[K]] = {
    val h = SeqPreHandler[Tok[D, *], FreeK[F, *], [i] =>> D, dom.IDelta]([i] => (d: D) => f(d)[i])
    ref match {
      case ref @ SimpleCellId(_) => observeF[F, [i] =>> D, [i] =>> U, dom.IDelta](ref)(h).map(_.fold(Subscription[FreeK[F, *]]())(subscription(ref, _)))
      case ref @ AutoCellId(_, _) => observeAutoF[F, [i] =>> D, [i] =>> U, dom.IDelta](ref)(h).map(_.fold(Subscription[FreeK[F, *]]())({ case (c, oid) => subscription(ref, c, oid) }))
    }
  }

  override def observeImplC[A, U, Δ, B](ref: Val[A])(f: A => ContU[FreeK[F, *], (Trigger[A, Δ], B)])(implicit
    dom: Dom.Aux[A, U, Δ],
  ): ContU[FreeK[F, *], (Subscription[FreeK[F, *]], B)] =
    IndexedContT((k: ((Subscription[FreeK[F, *]], B)) => FreeK[F, Unit]) =>
      ref match {
        case ref @ SimpleCellId(_) =>
          holdF[F, [i] =>> A](ref)([i] => (a: A, t: Token[i], oid: ObserverId) =>
            f(a).run(Id { case (tr, b) => resumeF[F, [i] =>> A, dom.IDelta, i](ref, t, tr[i]) >> k((subscription(ref, oid), b)) })
          ).void
        case ref @ AutoCellId(_, _) =>
          holdAutoF[F, [i] =>> A](ref)([i] => (a: A, cycle: CellCycle[[i] =>> A], t: Token[i], oid: ObserverId) =>
            f(a).run(Id { case (tr, b) => resumeAutoF[F, [i] =>> A, dom.IDelta, i](ref, cycle, t, tr[i]) >> k((subscription(ref, cycle, oid), b)) })
          ).void
      })

  private def subscription[D](ref: Var[D], oid: ObserverId): Subscription[FreeK[F, *]] =
    Subscription(rmObserverF(ref, oid))

  private def subscription[D](ref: AutoCellId[K, [i] =>> D], cycle: CellCycle[[i] =>> D], oid: ObserverId): Subscription[FreeK[F, *]] =
    Subscription(rmAutoObserverF(ref, cycle, oid))

  override def discard[A, Δ]: Trigger[A, Δ] =
    new Forall[TriggerI[A, Δ, *]] {
      override def compute[I]: TriggerI[A, Δ, I] = SeqTrigger.Discard[Tok[A, *], FreeK[F, *], [i] =>> A, [i, j] =>> Δ, I]()
    }
  override def fire[A, Δ](action: FreeK[F, Unit]): Trigger[A, Δ] =
    new Forall[TriggerI[A, Δ, *]] {
      override def compute[I]: TriggerI[A, Δ, I] = SeqTrigger.Fire[Tok[A, *], FreeK[F, *], [i] =>> A, [i, j] =>> Δ, I](action)
    }
  override def sleep[A, Δ](next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    new Forall[TriggerI[A, Δ, *]] {
      override def compute[I]: TriggerI[A, Δ, I] =
        SeqTrigger.Sleep[Tok[A, *], FreeK[F, *], [i] =>> A, [i, j] =>> Δ, I](
          SeqHandler[Tok[A, *], K, [i] =>> A, [i, j] =>> Δ, I](
            [j] => (a: A, δ: Δ) => next(a, δ)[j]
          )
        )
    }
  override def fireReload[A, Δ](action: FreeK[F, Unit], next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    new Forall[TriggerI[A, Δ, *]] {
      override def compute[I]: TriggerI[A, Δ, I] =
        SeqTrigger.FireReload[Tok[A, *], FreeK[F, *], [i] =>> A, [i, j] =>> Δ, I](
          action,
          SeqHandler[Tok[A, *], K, [i] =>> A, [i, j] =>> Δ, I](
            [j] => (a: A, δ: Δ) => next(a, δ)[j]
          )
        )
    }
  override def reconsider[A, Δ](action: FreeK[F, Trigger[A, Δ]]): Trigger[A, Δ] =
    new Forall[TriggerI[A, Δ, *]] {
      override def compute[I]: TriggerI[A, Δ, I] =
        SeqTrigger.Reconsider[Tok[A, *], FreeK[F, *], [i] =>> A, [i, j] =>> Δ, I](tok => tok._1 match {
          case -\/(r)      => action >>= (tr => resumeF    [F, [i] =>> A, [i, j] =>> Δ, I](r,    tok._2, tr[I]))
          case \/-((r, c)) => action >>= (tr => resumeAutoF[F, [i] =>> A, [i, j] =>> Δ, I](r, c, tok._2, tr[I]))
        })
    }
}