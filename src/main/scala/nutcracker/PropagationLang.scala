package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK}
import scalaz.~>
import scalaz.Id._
import shapeless.HList

sealed trait PropagationLang[Ref[_], Tok[_], K[_], A] {
  protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Tok, L, A]
}

object PropagationLang {

  private type FP[Ref[_], Tok[_], A] = FreeK[PropagationLang[Ref, Tok, ?[_], ?], A]

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], Tok[_], K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, K, Ref[D]] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Tok, L, Ref[D]] = NewCell(d, dom)
  }
  case class Update[Ref[_], Tok[_], K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Tok, L, Unit] = Update[Ref, Tok, L, D, U, Δ](ref, u, dom)
  }
  case class Observe[Ref[_], Tok[_], K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Tok, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Tok, L, Unit] = Observe[Ref, Tok, L, D, U, Δ](ref, f.map(tr(_)), dom)
  }
  case class Resume[Ref[_], Tok[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Tok, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Tok, L, Unit] = Resume[Ref, Tok, L, D, U, Δ, D0](ref, token, handler.map(tr(_)), dom)
  }
  case class SelTrigger[Ref[_], Tok[_], K[_], L <: HList](sel: Sel[Ref, L], f: L => (Option[K[Unit]], Boolean)) extends PropagationLang[Ref, Tok, K, Unit] {
    protected def transform[K2[_]](tr: K ~> K2): PropagationLang[Ref, Tok, K2, Unit] = selTrigger(sel){ l => {
      val (ko, b) = f(l)
      (ko.map(tr(_)), b)
    }}
  }

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], Tok[_], K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, Tok, K, Ref[D]] =
    NewCell[Ref, Tok, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], Tok[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, K, Unit] =
    Update[Ref, Tok, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], Tok[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Tok, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, K, Unit] =
    Observe[Ref, Tok, K, D, U, Δ](ref, f, dom)
  def resume[Ref[_], Tok[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Tok, K, Unit] =
    Resume[Ref, Tok, K, D, U, Δ, D0](ref, token, handler, dom)
  def selTrigger[Ref[_], Tok[_], K[_], L <: HList](sel: Sel[Ref, L])(f: L => (Option[K[Unit]], Boolean)): PropagationLang[Ref, Tok, K, Unit] =
    SelTrigger(sel, f)

  def updateF[F[_[_], _], Ref[_], Tok[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(update[Ref, Tok, FreeK[F, ?], D, U, Δ](ref)(u))
  def resumeF[F[_[_], _], Ref[_], Tok[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Tok[D0], handler: SeqHandler[Tok, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Tok, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, Tok, FreeK[F, ?], D, U, Δ, D0](ref, token, handler))


  implicit def functorKInstance[Ref[_], Tok[_]]: FunctorKA[PropagationLang[Ref, Tok, ?[_], ?]] = new FunctorKA[PropagationLang[Ref, Tok, ?[_], ?]] {
    def transform[K[_], L[_], A](pk: PropagationLang[Ref, Tok, K, A])(tr: K ~> L): PropagationLang[Ref, Tok, L, A] = pk.transform(tr)
  }

  implicit def freePropagation[Ref[_], Tok[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, Tok, F]
}


private[nutcracker] class FreePropagation[Ref[_], Tok[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Tok, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(PropagationLang.newCell[Ref, Tok, FreeK[F, ?], D](d))

  def updateImpl[D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    PropagationLang.updateF[F, Ref, Tok, D, U, dom.IDelta](ref)(u)

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => Trigger[FreeK[F, ?], D, Δ])(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] = {
    import SeqTrigger._

    def seqHandler[D1](f: (D, Δ) => Trigger[FreeK[F, ?], D, Δ]): SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] =
      new SeqHandler[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D1] {
        def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2] =
          f(d2, δ).unfix match {
            case TriggerF.Discard() => Discard[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2]()
            case TriggerF.Fire(k) => Fire[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](k)
            case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](seqHandler(next))
            case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, λ[(α, β) => Δ], D2](
              (token: Tok[D2]) => cont >>= { h =>
                PropagationLang.resumeF[F, Ref, Tok, D, U, λ[(α, β) => Δ], D2](ref, token, seqHandler(h))
              }
            )
          }
      }

    FreeK.injLiftF(PropagationLang.observe[Ref, Tok, FreeK[F, ?], D, U, dom.IDelta](ref)(new SeqPreHandler[Tok, FreeK[F, Unit], D, dom.IDelta] {
      def handle[D0 <: D](d: D0): SeqTrigger[Tok, FreeK[F, Unit], D, dom.IDelta, D0] = {
        f(d).unfix match {
          case TriggerF.Discard() => Discard()
          case TriggerF.Fire(k) => Fire(k)
          case TriggerF.Sleep(next) => Sleep[Tok, FreeK[F, Unit], D, dom.IDelta, D0](seqHandler(next))
          case TriggerF.FireReload(cont) => FireReload[Tok, FreeK[F, Unit], D, dom.IDelta, D0](
            token => cont >>= (h => PropagationLang.resumeF[F, Ref, Tok, D, U, dom.IDelta, D0](ref, token, seqHandler(h))))
        }
      }
    }))
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

    FreeK.injLiftF(PropagationLang.selTrigger[Ref, Tok, FreeK[F, ?], L](sel)(g))
  }
}