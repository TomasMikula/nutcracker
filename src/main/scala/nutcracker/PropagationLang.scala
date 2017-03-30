package nutcracker

import nutcracker.util.typealigned.BoundedAPair
import nutcracker.util.{FreeK, InjectK, Mediated}
import scala.language.higherKinds
import scalaz.~>
import scalaz.Id._
import shapeless.HList

private[nutcracker] sealed trait PropagationLang[Ref[_], Tok[_], ObsId, K[_], A]

private[nutcracker] object PropagationLang {

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], Tok[_], ObsId, K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Ref[D]]
  case class Update[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Unit]
  case class Observe[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Tok, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Option[ObsId]]
  case class Hold[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D]) extends PropagationLang[Ref, Tok, ObsId, K, (BoundedAPair[D, Id, Tok], ObsId)]
  case class Resume[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, ObsId, K, Unit] {
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
  def hold[Ref[_], Tok[_], ObsId, K[_], D](ref: Ref[D]): PropagationLang[Ref, Tok, ObsId, K, (BoundedAPair[D, Id, Tok], ObsId)] =
    Hold[Ref, Tok, ObsId, K, D](ref)
  def resume[Ref[_], Tok[_], ObsId, K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, ObsId, K, Unit] =
    Resume[Ref, Tok, ObsId, K, D, U, Δ, D0](ref, token, handler, dom)
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
  def holdF[F[_[_], _], Ref[_], Tok[_], ObsId, D](ref: Ref[D])(implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, (BoundedAPair[D, Id, Tok], ObsId)] =
    FreeK.injLiftF(hold[Ref, Tok, ObsId, FreeK[F, ?], D](ref))
  def resumeF[F[_[_], _], Ref[_], Tok[_], ObsId, D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, Tok, ObsId, FreeK[F, ?], D, U, Δ, D0](ref, token, handler))
  def rmObserverF[F[_[_], _], Ref[_], Tok[_], ObsId, D](ref: Ref[D], oid: ObsId)(implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(rmObserver[Ref, Tok, ObsId, FreeK[F, ?], D](ref, oid))


  implicit def freePropagation[Ref[_], Tok[_], ObsId, F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, Tok, ObsId, F]
}


private[nutcracker] class FreePropagation[Ref[_], Tok[_], ObsId, F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ObsId, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {
  import SeqTrigger._

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    PropagationLang.newCellF[F, Ref, Tok, ObsId, D](d)

  def updateImpl[D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): FreeK[F, Unit] =
    PropagationLang.updateF[F, Ref, Tok, ObsId, D, U, Δ](ref)(u)

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Subscription[FreeK[F, ?]]] = {
    PropagationLang.observeF[F, Ref, Tok, ObsId, D, U, dom.IDelta](ref)(new SeqPreHandler[Tok, FreeK[F, Unit], D, dom.IDelta] {
      def handle[D0 <: D](d: D0): SeqTrigger[Tok, FreeK[F, Unit], D, dom.IDelta, D0] = {
        f(d).unfix match {
          case TriggerF.Discard() => Discard()
          case TriggerF.Fire(k) => Fire(k)
          case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, dom.IDelta, D0](seqHandler(ref, next))
          case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, dom.IDelta, D0](
            token => cont >>= (h => PropagationLang.resumeF[F, Ref, Tok, ObsId, D, U, dom.IDelta, D0](ref, token, seqHandler(ref, h))))
        }
      }
    }).map(_.fold(Subscription[FreeK[F, ?]]())(subscription(ref, _)))
  }

  override def peek[D](ref: Ref[D])(implicit dom: Dom[D]): Mediated[FreeK[F, ?], D, (D, dom.Delta) => Trigger[FreeK[F, ?], D, dom.Delta], Subscription[FreeK[F, ?]]] =
    peek0[D, dom.Update, dom.Delta](ref)(dom)

  @inline
  private def peek0[D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]): Mediated[FreeK[F, ?], D, (D, Δ) => Trigger[FreeK[F, ?], D, Δ], Subscription[FreeK[F, ?]]] =
    Mediated(PropagationLang.holdF(ref) map { case (dt, oid) =>
      (dt._1, handler => PropagationLang.resumeF[F, Ref, Tok, ObsId, D, U, dom.IDelta, dt.A](ref, dt._2, seqHandler(ref, handler)) map (_ =>  subscription(ref, oid)))
    })

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
    Subscription(PropagationLang.rmObserverF(ref, oid))

  private def seqHandler[D, U, Δ, D1](ref: Ref[D], f: (D, Δ) => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
    new SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] {
      def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2] =
        f(d2, δ).unfix match {
          case TriggerF.Discard() => Discard[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2]()
          case TriggerF.Fire(k) => Fire[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](k)
          case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](seqHandler(ref, next))
          case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](
            (token: Tok[D2]) => cont >>= { h =>
              PropagationLang.resumeF[F, Ref, Tok, ObsId, D, U, λ[(α, β) => Δ], D2](ref, token, seqHandler(ref, h))
            }
          )
        }
    }
}