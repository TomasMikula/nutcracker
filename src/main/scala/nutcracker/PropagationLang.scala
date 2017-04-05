package nutcracker

import nutcracker.util.{ContU, FreeK, FreeKT, InjectK}
import scalaz.{Bind, IndexedContT, Value, ~>}
import scalaz.Id._
import scalaz.syntax.monad._
import shapeless.HList

private[nutcracker] sealed trait PropagationLang[Ref[_], K[_], A]

private[nutcracker] object PropagationLang {

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Ref[D]]
  case class Update[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit]
  case class Observe[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Token, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Option[ObserverId]]
  // TODO: Still need to return Value? Still need Bind[K]? Still need supply? Still need U, Δ?
  case class Hold[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D], f: (D, Token[D], ObserverId) => K[Unit], supply: (CellCycle[D], D) => K[Unit], bnd: Bind[K], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Value[ObserverId]]
  case class Supply[Ref[_], K[_], D](ref: Ref[D], cycle: CellCycle[D], value: D) extends PropagationLang[Ref, K, Unit]
  case class Resume[Ref[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit] {
    type Arg = D0
  }
  case class Triggered[Ref[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit] {
    type Arg = D0
  }
  case class RmObserver[Ref[_], K[_], D](ref: Ref[D], oid: ObserverId) extends PropagationLang[Ref, K, Unit]
  case class SelTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L], f: L => (Option[K[Unit]], Boolean)) extends PropagationLang[Ref, K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, K, Ref[D]] =
    NewCell[Ref, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Update[Ref, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Option[ObserverId]] =
    Observe[Ref, K, D, U, Δ](ref, f, dom)
  def hold[Ref[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit bnd: Bind[K], dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Value[ObserverId]] =
    Hold[Ref, K, D, U, Δ](ref, f, supply, bnd, dom)
  def supply[Ref[_], K[_], D](ref: Ref[D])(cycle: CellCycle[D], value: D): PropagationLang[Ref, K, Unit] =
    Supply(ref, cycle, value)
  def resume[Ref[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Resume[Ref, K, D, U, Δ, D0](ref, token, handler, dom)
  def triggered[Ref[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Triggered[Ref, K, D, U, Δ, D0](ref, token, trigger, dom)
  def rmObserver[Ref[_], K[_], D](ref: Ref[D], oid: ObserverId): PropagationLang[Ref, K, Unit] =
    RmObserver(ref, oid)
  def selTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L])(f: L => (Option[K[Unit]], Boolean)): PropagationLang[Ref, K, Unit] =
    SelTrigger(sel, f)


  // constructors injected into free programs

  def newCellF[F[_[_], _], Ref[_], D](d: D)(implicit dom: Dom[D], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(newCell[Ref, FreeK[F, ?], D](d))

  def updateF[F[_[_], _], Ref[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(update[Ref, FreeK[F, ?], D, U, Δ](ref)(u))

  def observeF[F[_[_], _], Ref[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Token, FreeK[F, Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Option[ObserverId]] =
    FreeK.injLiftF(observe[Ref, FreeK[F, ?], D, U, Δ](ref)(f))

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def holdF[F[_[_], _], Ref[_], D, U, Δ[_, _]](ref: Ref[D])(f: (D, Token[D], ObserverId) => FreeK[F, Unit])(supply: (CellCycle[D], D) => FreeK[F, Unit])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, ObserverId] =
    FreeK.injLiftF(hold[Ref, FreeK[F, ?], D, U, Δ](ref)(f)(supply)(FreeKT.freeKTMonad[F, Id], dom)).map(_.value)

  def supplyF[F[_[_], _], Ref[_], D](ref: Ref[D])(cycle: CellCycle[D], value: D)(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(supply[Ref, FreeK[F, ?], D](ref)(cycle, value))

  def resumeF[F[_[_], _], Ref[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, FreeK[F, ?], D, U, Δ, D0](ref, token, handler))

  def triggeredF[F[_[_], _], Ref[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], trigger: SeqTrigger[Token, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(triggered[Ref, FreeK[F, ?], D, U, Δ, D0](ref, token, trigger))

  def rmObserverF[F[_[_], _], Ref[_], D](ref: Ref[D], oid: ObserverId)(implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(rmObserver[Ref, FreeK[F, ?], D](ref, oid))


  implicit def freePropagation[Ref[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, F]
}


private[nutcracker] class FreePropagation[Ref[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {
  import PropagationLang._
  import SeqTrigger._

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    newCellF[F, Ref, D](d)

  def updateImpl[D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): FreeK[F, Unit] =
    updateF[F, Ref, D, U, Δ](ref)(u)

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[FreeK[F, ?]]] = {
    observeF[F, Ref, D, U, dom.IDelta](ref)(new SeqPreHandler[Token, FreeK[F, Unit], D, dom.IDelta] {
      def handle[D0 <: D](d: D0): SeqTrigger[Token, FreeK[F, Unit], D, dom.IDelta, D0] = {
        f(d).unfix match {
          case TriggerF.Discard() => Discard()
          case TriggerF.Fire(k) => Fire(k)
          case TriggerF.Sleep(next) => Sleep[Token, FreeK[F, Unit], D, dom.IDelta, D0](seqHandler(ref, next))
          case TriggerF.FireReload(cont) => FireReload[Token, FreeK[F, Unit], D, dom.IDelta, D0](
            token => cont >>= (h => resumeF[F, Ref, D, U, dom.IDelta, D0](ref, token, seqHandler(ref, h))))
        }
      }
    }).map(_.fold(Subscription[FreeK[F, ?]]())(subscription(ref, _)))
  }



  override def observeImplC[A, U, Δ, B](src: Ref[A])(f: A => ContU[FreeK[F, ?], (Trigger[FreeK[F, ?], A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[FreeK[F, ?], (Subscription[FreeK[F, ?]], B)] = {
    IndexedContT((k: ((Subscription[FreeK[F, ?]], B)) => FreeK[F, Unit]) =>
      holdF[F, Ref, A, U, dom.IDelta](src)((a: A, t: Token[A], oid: ObserverId) =>
        f(a).run({ case (tr, b) => triggeredF[F, Ref, A, U, dom.IDelta, A](src, t, seqTrigger(src, tr)) >> k((subscription(src, oid), b)) })
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

    FreeK.injLiftF(PropagationLang.selTrigger[Ref, FreeK[F, ?], L](sel)(g))
  }

  private def subscription[D](ref: Ref[D], oid: ObserverId): Subscription[FreeK[F, ?]] =
    Subscription(rmObserverF(ref, oid))

  private def seqHandler[D, U, Δ, D1](ref: Ref[D], f: (D, Δ) => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): SeqHandler[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
    new SeqHandler[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] {
      def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D2] =
        seqTrigger(ref, f(d2, δ))
    }

  private def seqTrigger[D, U, Δ, D1 <: D](ref: Ref[D], trigger: Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): SeqTrigger[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
    trigger.unfix match {
      case TriggerF.Discard() => Discard[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1]()
      case TriggerF.Fire(k) => Fire[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](k)
      case TriggerF.Sleep(next) => Sleep[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](seqHandler(ref, next))
      case TriggerF.FireReload(cont) => FireReload[Token, FreeK[F, Unit], D, λ[(α, β) => Δ], D1](
        (token: Token[D1]) => cont >>= { h =>
          resumeF[F, Ref, D, U, λ[(α, β) => Δ], D1](ref, token, seqHandler(ref, h))
        }
      )
    }
}