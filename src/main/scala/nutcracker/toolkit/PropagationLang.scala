package nutcracker.toolkit

import nutcracker.util.{ContU, Exists, Forall, FreeK, Id, IndexedContT, Inject}
import nutcracker.{Dom, IDom, OnDemandPropagation, SeqHandler, SeqPreHandler, SeqTrigger, Subscription}
import scalaz.{-\/, \/, \/-, Monad}
import scalaz.syntax.functor._

private[nutcracker] sealed trait PropagationLang[K[_], A]

private[nutcracker] object PropagationLang {

  // constructors (the instruction set of a free program)
  case class NewCell[K[_], D[_], I](d: D[I], dom: IDom[D]) extends PropagationLang[K, SimpleCellId[K, D]]
  case class Update[K[_], D[_], U[_], Res[_, _], J](ref: SimpleCellId[K, D], u: U[J], dom: IDom.Aux1[D, U, Res]) extends PropagationLang[K, Exists[Res[*, J]]]
  case class Observe[K[_], D[_], U[_], Δ[_, _]](ref: SimpleCellId[K, D], f: SeqPreHandler[K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[ObserverId]]
  case class Hold[K[_], D[_]](ref: SimpleCellId[K, D], f: [i] => (D[i], Token[i], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class Resume[K[_], D[_], Δ[_, _], I](ref: SimpleCellId[K, D], token: Token[I], trigger: SeqTrigger[K, D, Δ, I]) extends PropagationLang[K, Unit] {
    type Domain[i] = D[i]
    type Delta[i, j] = Δ[i, j]
    type Idx = I
  }
  case class RmObserver[K[_], D[_]](ref: SimpleCellId[K, D], oid: ObserverId) extends PropagationLang[K, Unit]

  case class NewAutoCell[K[_], D[_]](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]) extends PropagationLang[K, CellId[K, D]]
  case class ObserveAuto[K[_], D[_], U[_], Δ[_, _]](ref: AutoCellId[K, D], f: SeqPreHandler[K, D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[K, Option[(CellCycle[D], ObserverId)]]
  case class HoldAuto[K[_], D[_]](ref: AutoCellId[K, D], f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]) extends PropagationLang[K, ObserverId]
  case class ResumeAuto[K[_], D[_], Δ[_, _], I](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[I], trigger: SeqTrigger[K, D, Δ, I]) extends PropagationLang[K, Unit] {
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
    NewCell[K, D, I](d, dom)
  def update[K[_], D[_], U[_], Res[_, _], J](ref: SimpleCellId[K, D])(u: U[J])(implicit dom: IDom.Aux1[D, U, Res]): PropagationLang[K, Exists[Res[*, J]]] =
    Update[K, D, U, Res, J](ref, u, dom)
  def observe[K[_], D[_], U[_], Δ[_, _]](ref: SimpleCellId[K, D])(f: SeqPreHandler[K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[ObserverId]] =
    Observe[K, D, U, Δ](ref, f, dom)
  def observeAuto[K[_], D[_], U[_], Δ[_, _]](ref: AutoCellId[K, D])(f: SeqPreHandler[K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[K, Option[(CellCycle[D], ObserverId)]] =
    ObserveAuto[K, D, U, Δ](ref, f, dom)
  def hold[K[_], D[_]](ref: SimpleCellId[K, D])(f: [i] => (D[i], Token[i], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    Hold[K, D](ref, f)
  def holdAuto[K[_], D[_]](ref: AutoCellId[K, D])(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): PropagationLang[K, ObserverId] =
    HoldAuto[K, D](ref, f)
  def supply[K[_], D[_], I](ref: AutoCellId[K, D])(cycle: CellCycle[D], value: D[I]): PropagationLang[K, Unit] =
    Supply(ref, cycle, value)
  def resume[K[_], D[_], Δ[_, _], I0](ref: SimpleCellId[K, D], token: Token[I0], trigger: SeqTrigger[K, D, Δ, I0]): PropagationLang[K, Unit] =
    Resume[K, D, Δ, I0](ref, token, trigger)
  def resumeAuto[K[_], D[_], Δ[_, _], I0](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[I0], trigger: SeqTrigger[K, D, Δ, I0]): PropagationLang[K, Unit] =
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

  def updateF[F[_[_], _], D[_], U[_], Res[_, _], J](ref: SimpleCellId[FreeK[F, *], D])(u: U[J])(implicit dom: IDom.Aux1[D, U, Res], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Exists[Res[*, J]]] =
    FreeK.liftF(inj(update[FreeK[F, *], D, U, Res, J](ref)(u)))

  def observeF[F[_[_], _], D[_], U[_], Δ[_, _]](ref: SimpleCellId[FreeK[F, *], D])(f: SeqPreHandler[FreeK[F, *], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Option[ObserverId]] =
    FreeK.liftF(inj(observe[FreeK[F, *], D, U, Δ](ref)(f)))

  def observeAutoF[F[_[_], _], D[_], U[_], Δ[_, _]](ref: AutoCellId[FreeK[F, *], D])(f: SeqPreHandler[FreeK[F, *], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Option[(CellCycle[D], ObserverId)]] =
    FreeK.liftF(inj(observeAuto[FreeK[F, *], D, U, Δ](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], D[_]](ref: SimpleCellId[FreeK[F, *], D])(f: [i] => (D[i], Token[i], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(hold[FreeK[F, *], D](ref)(f)))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdAutoF[F[_[_], _], D[_]](ref: AutoCellId[FreeK[F, *], D])(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => FreeK[F, Unit])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, ObserverId] =
    FreeK.liftF(inj(holdAuto[FreeK[F, *], D](ref)(f)))

  def supplyF[F[_[_], _], D[_], I](ref: AutoCellId[FreeK[F, *], D])(cycle: CellCycle[D], value: D[I])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(supply[FreeK[F, *], D, I](ref)(cycle, value)))

  def resumeF[F[_[_], _], D[_], Δ[_, _], I0](ref: SimpleCellId[FreeK[F, *], D], token: Token[I0], trigger: SeqTrigger[FreeK[F, *], D, Δ, I0])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
    FreeK.liftF(inj(resume[FreeK[F, *], D, Δ, I0](ref, token, trigger)))

  def resumeAutoF[F[_[_], _], D[_], Δ[_, _], I0](ref: AutoCellId[FreeK[F, *], D], cycle: CellCycle[D], token: Token[I0], trigger: SeqTrigger[FreeK[F, *], D, Δ, I0])(implicit inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]]): FreeK[F, Unit] =
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
  ): FreePropagation[F] =
    new FreePropagation[F]
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit
  inj: Inject[PropagationLang[FreeK[F, *], *], F[FreeK[F, *], *]],
) extends OnDemandPropagation[FreeK[F, *]] {
  import PropagationLang._

  type K[A] = FreeK[F, A]

  override type IVar[A[_]] = SimpleCellId[K, A]
  override type IVal[A[_]] = CellId[K, A]

  override type Out[A] = nutcracker.toolkit.Out[[d[_]] =>> SimpleCellId[K, d], A]

  type CellCycleId[D[_]] = IVar[D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[D[_], I] = (CellCycleId[D], Token[I])

  override type ITrigger[D[_], Δ[_, _], I] = SeqTrigger[K, D, Δ, I]
  override type ExclRef[A] = (AutoCellId[K, [i] =>> A], CellCycle[[i] =>> A])

  override def M: Monad[FreeK[F, *]] =
    FreeK.freeKMonad[F]

  override def iReadOnly[D[_]](ref: IVar[D]): IVal[D] =
    ref

  override def readOnly[A](ref: Var[A]): Val[A] =
    iReadOnly[[i] =>> A](ref)

  override def newICell[D[_], I](d: D[I])(implicit dom: IDom[D]): FreeK[F, IVar[D]] =
    newCellF(d)

  override def newAutoCellC[A](setup: IndexedContT[Unit, ExclRef[A], K, A])(implicit dom: Dom[A]): FreeK[F, Val[A]] = {
    val setup1 = (r: AutoCellId[K, [i] =>> A], c: CellCycle[[i] =>> A]) => setup.run(Id(a => supplyF(r)(c, a).as((r, c))))
    newAutoCellF(setup1)
  }

  override def addFinalizer[A](ref: ExclRef[A], value: Subscription[K]): FreeK[F, Subscription[K]] =
    addFinalizerF(ref._1, ref._2, value) map {
      case Some(fid) => Subscription(removeFinalizerF(ref._1, ref._2, fid))
      case None      => Subscription()
    }

  override def iUpdate[D[_], U[_], J](ref: IVar[D])(u: U[J])(implicit dom: IDom.Aux0[D, U]): FreeK[F, Exists[dom.IUpdateRes[*, J]]] =
    updateF[F, D, U, dom.IUpdateRes, J](ref)(u)

  override def exclUpdateImpl[A, U, Δ](ref: ExclRef[A], u: U)(implicit dom: Dom.Aux[A, U, Δ]): FreeK[F, Unit] =
    exclUpdateF[F, [i] =>> A, [i] =>> U, [i, j] =>> Δ, Any](ref._1, ref._2, u)

  override def iObserve[D[_], Δ[_, _]](ref: IVal[D], f: [i] => D[i] => ITrigger[D, Δ, i])(using dom: IDom.AuxΔ[D, Δ]): FreeK[F, Subscription[K]] = {
    val h = SeqPreHandler[FreeK[F, *], D, Δ](f)
    ref match {
      case ref @ SimpleCellId(_) => observeF[F, D, dom.IUpdate, Δ](ref)(h).map(_.fold(Subscription[FreeK[F, *]]())(subscription(ref, _)))
      case ref @ AutoCellId(_, _) => observeAutoF[F, D, dom.IUpdate, Δ](ref)(h).map(_.fold(Subscription[FreeK[F, *]]())({ case (c, oid) => subscription(ref, c, oid) }))
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

  private def subscription[D[_]](ref: IVar[D], oid: ObserverId): Subscription[FreeK[F, *]] =
    Subscription(rmObserverF(ref, oid))

  private def subscription[D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): Subscription[FreeK[F, *]] =
    Subscription(rmAutoObserverF(ref, cycle, oid))

  override def iDiscard[A[_], Δ[_, _], I]: ITrigger[A, Δ, I] =
    SeqTrigger.Discard[FreeK[F, *], A, Δ, I]()

  override def iFire[A[_], Δ[_, _], I](action: FreeK[F, Unit]): ITrigger[A, Δ, I] =
    SeqTrigger.Fire[FreeK[F, *], A, Δ, I](action)

  override def iSleep[A[_], Δ[_, _], I](next: [j] => (A[j], Δ[I, j]) => ITrigger[A, Δ, j]): ITrigger[A, Δ, I] =
    SeqTrigger.Sleep[FreeK[F, *], A, Δ, I](
      SeqHandler[K, A, Δ, I](
        next
      )
    )

  override def iFireReload[A[_], Δ[_, _], I](action: FreeK[F, Unit], next: [j] => (A[j], Δ[I, j]) => ITrigger[A, Δ, j]): ITrigger[A, Δ, I] =
    SeqTrigger.FireReload[FreeK[F, *], A, Δ, I](
      action,
      SeqHandler[K, A, Δ, I](
        next
      )
    )

  override def iReconsider[A[_], Δ[_, _], I](action: FreeK[F, ITrigger[A, Δ, I]]): ITrigger[A, Δ, I] =
    SeqTrigger.Reconsider[FreeK[F, *], A, Δ, I](f => action >>= (tr => f(tr)))

  extension [D[_], Δ[_, _], I](t: ITrigger[D, Δ, I]) {
    def contramap[C[_], Γ[_, _]](
      f: [i] => C[i] => D[i],
      g: [i, j] => Γ[i, j] => Δ[i, j],
    ): ITrigger[C, Γ, I] =
      SeqTrigger.contramap(t)(f, g)
  }

  override def iOut[A[_], B](a: IVar[A], f: [i] => A[i] => B): Out[B] =
    Out.WrapIVar(a, f)

  override def constOut[A](a: A): Out[A] =
    Out.Const(a)

  override def mapOut[A, B](a: Out[A])(f: A => B): Out[B] =
    Out.Mapped(a, f)

  override def pairOut[A, B](a: Out[A], b: Out[B]): Out[(A, B)] =
    Out.Pair(a, b)

  override def flatMapOut[A, B](a: Out[A])(f: A => Out[B]): Out[B] =
    Out.FlatMap(a, f)
}

private[nutcracker] sealed trait Out[IVar[_[_]], A]
private[nutcracker] object Out {
  case class Const[IVar[_[_]], A](value: A) extends Out[IVar, A]
  case class WrapIVar[IVar[_[_]], A[_], B](v: IVar[A], f: [i] => A[i] => B) extends Out[IVar, B]
  case class Mapped[IVar[_[_]], A, B](v: Out[IVar, A], f: A => B) extends Out[IVar, B]
  case class Pair[IVar[_[_]], A, B](a: Out[IVar, A], b: Out[IVar, B]) extends Out[IVar, (A, B)]
  case class FlatMap[IVar[_[_]], A, B](a: Out[IVar, A], f: A => Out[IVar, B]) extends Out[IVar, B]
}