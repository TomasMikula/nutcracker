package nutcracker

import nutcracker.util.{ContU, FreeK, FreeKT, InjectK}
import scalaz.{Bind, IndexedContT, Value, ~>}
import scalaz.Id._
import scalaz.syntax.monad._
import shapeless.HList

private[nutcracker] sealed trait PropagationLang[Ref[_], Tok[_], ObsId, K[_], A]

private[nutcracker] object PropagationLang {

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], Tok[_], ObsId, K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Ref[D]]
  case class Update[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Unit]
  case class Observe[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Tok, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Option[ObsId]]
  // TODO: Still need to return Value? Still need Bind[K]? Still need supply? Still need U, Δ?
  case class Hold[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D], f: (D, Tok[D], ObsId) => K[Unit], supply: (CellCycle[D], D) => K[Unit], bnd: Bind[K], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Value[ObsId]]
  case class Supply[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D], cycle: CellCycle[D], value: D) extends PropagationLang[Ref, Tok, ObsId, K, Unit]
  case class Resume[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Unit] {
    type Arg = D0
  }
  case class Triggered[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], trigger: SeqTrigger[Tok, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Unit] {
    type Arg = D0
  }
  case class RmObserver[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D], oid: ObsId) extends PropagationLang[Ref, Tok, ObsId, K, Unit]
  case class SelTrigger[Ref[_], Tok[_], ObsId, K[_], L <: HList](sel: Sel[Ref, L], f: L => (Option[K[Unit]], Boolean)) extends PropagationLang[Ref, Tok, ObsId, K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], Tok[_], ObsId, K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, Tok, ObsId, K, Ref[D]] =
    NewCell[Ref, Tok, ObsId, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    Update[Ref, Tok, ObsId, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Tok, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Option[ObsId]] =
    Observe[Ref, Tok, ObsId, K, D, U, Δ](ref, f, dom)
  def hold[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D])(f: (D, Tok[D], ObsId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit bnd: Bind[K], dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Value[ObsId]] =
    Hold[Ref, Tok, ObsId, K, D, U, Δ](ref, f, supply, bnd, dom)
  def supply[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D])(cycle: CellCycle[D], value: D): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    Supply(ref, cycle, value)
  def resume[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    Resume[Ref, Tok, ObsId, K, D, U, Δ, D0](ref, token, handler, dom)
  def triggered[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], trigger: SeqTrigger[Tok, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    Triggered[Ref, Tok, ObsId, K, D, U, Δ, D0](ref, token, trigger, dom)
  def rmObserver[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D], oid: ObsId): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    RmObserver(ref, oid)
  def selTrigger[Ref[_], Tok[_], ObsId, K[_], L <: HList](sel: Sel[Ref, L])(f: L => (Option[K[Unit]], Boolean)): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    SelTrigger(sel, f)


  // constructors injected into free programs

  def newCellF[F[_[_], _], Ref[_], Tok[_], ObsId, D](d: D)(implicit dom: Dom[D], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(newCell[Ref, Tok, ObsId, FreeK[F, ?], D](d))

  def updateF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(update[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ](ref)(u))

  def observeF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Tok, FreeK[F, Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Option[ObsId]] =
    FreeK.injLiftF(observe[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ](ref)(f))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _]](ref: Ref[D])(f: (D, Tok[D], ObsId) => FreeK[F, Unit])(supply: (CellCycle[D], D) => FreeK[F, Unit])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, ObsId] =
    FreeK.injLiftF(hold[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ](ref)(f)(supply)(FreeKT.freeKTMonad[F, Id], dom)).map(_.value)

  def supplyF[F[_[_], _], Ref[_], Tok[_], ObsId, D](ref: Ref[D])(cycle: CellCycle[D], value: D)(implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(supply[Ref, Tok, ObsId, FreeK[F, ?], D](ref)(cycle, value))

  def resumeF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ, D0](ref, token, handler))

  def triggeredF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], trigger: SeqTrigger[Tok, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(triggered[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ, D0](ref, token, trigger))

  def rmObserverF[F[_[_], _], Ref[_], Tok[_], ObsId, D](ref: Ref[D], oid: ObsId)(implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(rmObserver[Ref, Tok, ObsId, FreeK[F, ?], D](ref, oid))


  implicit def freePropagation[Ref[_], Tok[_], ObsId, F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, Tok, ObsId, F]
}


private[nutcracker] class FreePropagation[Ref[_], Tok[_], ObsId, F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {
  import PropagationLang._
  import SeqTrigger._

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    newCellF[F, Ref, Tok, ObsId, D](d)

  def updateImpl[D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): FreeK[F, Unit] =
    updateF[F, Ref, Tok, ObsId, D, U, Δ](ref)(u)

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[FreeK[F, ?]]] = {
    observeF[F, Ref, Tok, ObsId, D, U, dom.IDelta](ref)(new SeqPreHandler[Tok, FreeK[F, Unit], D, dom.IDelta] {
      def handle[D0 <: D](d: D0): SeqTrigger[Tok, FreeK[F, Unit], D, dom.IDelta, D0] = {
        f(d).unfix match {
          case TriggerF.Discard() => Discard()
          case TriggerF.Fire(k) => Fire(k)
          case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, dom.IDelta, D0](seqHandler(ref, next))
          case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, dom.IDelta, D0](
            token => cont >>= (h => resumeF[F, Ref, Tok, ObsId, D, U, dom.IDelta, D0](ref, token, seqHandler(ref, h))))
        }
      }
    }).map(_.fold(Subscription[FreeK[F, ?]]())(subscription(ref, _)))
  }



  override def observeImplC[A, U, Δ, B](src: Ref[A])(f: A => ContU[FreeK[F, ?], (Trigger[FreeK[F, ?], A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[FreeK[F, ?], (Subscription[FreeK[F, ?]], B)] = {
    IndexedContT((k: ((Subscription[FreeK[F, ?]], B)) => FreeK[F, Unit]) =>
      holdF[F, Ref, Tok, ObsId, A, U, dom.IDelta](src)((a: A, t: Tok[A], oid: ObsId) =>
        f(a).run({ case (tr, b) => triggeredF[F, Ref, Tok, ObsId, A, U, dom.IDelta, A](src, t, seqTrigger(src, tr)) >> k((subscription(src, oid), b)) })
      )(supplyF(src)(_, _)(inj))(dom, inj).void)
  }

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: Id ~> λ[α => L => TriggerF[FreeK[F, ?], α]]): FreeK[F, Unit] = {
    import TriggerF._

    val fu = f(())
    val g: L => (Option[FreeK[F, Unit]], Boolean) = l => {
      fu(l) match {
        case Discard() => (None, false)
        case Fire(k) => (Some(k), false)
        case Sleep(()) => (None, true)
        case FireReload(k) => (Some(k), true)
      }
    }

    FreeK.injLiftF(PropagationLang.selTrigger[Ref, Tok, ObsId, FreeK[F, ?], L](sel)(g))
  }

  private def subscription[D](ref: Ref[D], oid: ObsId): Subscription[FreeK[F, ?]] =
    Subscription(rmObserverF(ref, oid))

  private def seqHandler[D, U, Δ, D1](ref: Ref[D], f: (D, Δ) => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
    new SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] {
      def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2] =
        seqTrigger(ref, f(d2, δ))
    }

  private def seqTrigger[D, U, Δ, D1 <: D](ref: Ref[D], trigger: Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): SeqTrigger[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
    trigger.unfix match {
      case TriggerF.Discard() => Discard[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1]()
      case TriggerF.Fire(k) => Fire[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](k)
      case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](seqHandler(ref, next))
      case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](
        (token: Tok[D1]) => cont >>= { h =>
          resumeF[F, Ref, Tok, ObsId, D, U, λ[(α, β) => Δ], D1](ref, token, seqHandler(ref, h))
        }
      )
    }
}