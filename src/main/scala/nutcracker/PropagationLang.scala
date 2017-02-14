package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK}
import shapeless.HList

import scalaz.{Functor, ~>}

sealed trait PropagationLang[Ref[_], Token[_], K[_], A] {
  protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Token, L, A]
}

object PropagationLang {

  private type FP[Ref[_], Token[_], A] = FreeK[PropagationLang[Ref, Token, ?[_], ?], A]

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], Token[_], K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, Token, K, Ref[D]] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Token, L, Ref[D]] = NewCell(d, dom)
  }
  case class Update[Ref[_], Token[_], K[_], D, U, Δ[_, _]](ref: Ref[D], u: U, dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Token, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Token, L, Unit] = Update[Ref, Token, L, D, U, Δ](ref, u, dom)
  }
  case class Observe[Ref[_], Token[_], K[_], D, U, Δ[_, _]](ref: Ref[D], f: SeqPreHandler[Token, K[Unit], D, Δ], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Token, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Token, L, Unit] = Observe[Ref, Token, L, D, U, Δ](ref, f.map(tr(_)), dom)
  }
  case class Resume[Ref[_], Token[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0], dom: IDom.Aux[D, U, Δ]) extends PropagationLang[Ref, Token, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, Token, L, Unit] = Resume[Ref, Token, L, D, U, Δ, D0](ref, token, handler.map(tr(_)), dom)
  }
  case class SelTrigger[Ref[_], Token[_], K[_], L <: HList](sel: Sel[Ref, L], f: L => Trigger[K[Unit]]) extends PropagationLang[Ref, Token, K, Unit] {
    protected def transform[K2[_]](tr: K ~> K2): PropagationLang[Ref, Token, K2, Unit] = selTrigger(sel){ l => Functor[Trigger].map(f(l))(tr[Unit] _) }
  }

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], Token[_], K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, Token, K, Ref[D]] =
    NewCell[Ref, Token, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], Token[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Token, K, Unit] =
    Update[Ref, Token, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], Token[_], K[_], D, U, Δ[_, _]](ref: Ref[D])(f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Token, K, Unit] =
    Observe[Ref, Token, K, D, U, Δ](ref, f, dom)
  def resume[Ref[_], Token[_], K[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationLang[Ref, Token, K, Unit] =
    Resume[Ref, Token, K, D, U, Δ, D0](ref, token, handler, dom)
  def selTrigger[Ref[_], Token[_], K[_], L <: HList](sel: Sel[Ref, L])(f: L => Trigger[K[Unit]]): PropagationLang[Ref, Token, K, Unit] =
    SelTrigger(sel, f)

  def updateF[F[_[_], _], Ref[_], Token[_], D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Token, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(update[Ref, Token, FreeK[F, ?], D, U, Δ](ref)(u))
  def resumeF[F[_[_], _], Ref[_], Token[_], D, U, Δ[_, _], D0 <: D](ref: Ref[D], token: Token[D0], handler: SeqHandler[Token, FreeK[F, Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ], inj: InjectK[PropagationLang[Ref, Token, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF(resume[Ref, Token, FreeK[F, ?], D, U, Δ, D0](ref, token, handler))


  implicit def functorKInstance[Ref[_], Token[_]]: FunctorKA[PropagationLang[Ref, Token, ?[_], ?]] = new FunctorKA[PropagationLang[Ref, Token, ?[_], ?]] {
    def transform[K[_], L[_], A](pk: PropagationLang[Ref, Token, K, A])(tr: K ~> L): PropagationLang[Ref, Token, L, A] = pk.transform(tr)
  }

  implicit def freePropagation[Ref[_], Token[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Token, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, Token, F]
}


private[nutcracker] class FreePropagation[Ref[_], Token[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, Token, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(PropagationLang.newCell[Ref, Token, FreeK[F, ?], D](d))

  def updateImpl[D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    PropagationLang.updateF[F, Ref, Token, D, U, dom.IDelta](ref)(u)

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => (Option[FreeK[F, Unit]], Option[(D, Δ) => Trigger[FreeK[F, Unit]]]))(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.observe[Ref, Token, FreeK[F, ?], D, U, dom.IDelta](ref)(new SeqPreHandler[Token, FreeK[F, Unit], D, dom.IDelta] {
      def handle[D0 <: D](d: D0): (Option[FreeK[F, Unit]], Option[SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, D0]]) = {
        val (now, later) = f(d)
        (now, later.map(g => {
          val _seqHandler: SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, Any] =
            new SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, Any] { self =>
              def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Token, FreeK[F, Unit], D, dom.IDelta, D2] = {
                g(d2, δ) match {
                  case Trigger.Discard() => SeqTrigger.Discard()
                  case Trigger.Sleep() => SeqTrigger.Sleep(self.asInstanceOf[SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, D2]])
                  case Trigger.Fire(k) => SeqTrigger.Fire(k)
                  case Trigger.FireReload(k) => SeqTrigger.FireReload(token => k >> PropagationLang.resumeF(ref, token, self.asInstanceOf[SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, D2]]))
                }
              }
            }
          _seqHandler.asInstanceOf[SeqHandler[Token, FreeK[F, Unit], D, dom.IDelta, D0]]
        }))
      }
    }))

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: (L) => Trigger[FreeK[F, Unit]]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.selTrigger[Ref, Token, FreeK[F, ?], L](sel)(f))
}