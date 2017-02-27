package nutcracker

import scala.language.higherKinds
import scalaz.{Applicative, Bind, ~>}
import scalaz.syntax.bind._
import shapeless.{::, HList, HNil}
import Trigger._
import nutcracker.util.ops.applicative._

import scalaz.Id.Id

trait Propagation[M[_], Ref[_]] extends PSrc[Ref, M] {

  // basic instructions

  def newCell[D](d: D)(implicit dom: Dom[D]): M[Ref[D]]

  def updateImpl[D, U, Δ[_, _]](ref: Ref[D])(u: U)(implicit dom: IDom.Aux[D, U, Δ]): M[Unit]

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: Id ~> λ[α => L => TriggerF[M, α]]): M[Unit]


  def newCell[D](implicit dom: DomWithBottom[D]): M[Ref[D]] =
    newCell(dom.bottom)

  def update[D](ref: Ref[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl[D, U, λ[(α, β) => Δ]](ref)(u)
  }


  // derived methods

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    newCell(d).replicate(n)

  def selTrigger2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: Id ~> λ[α => (D1, D2) => TriggerF[M, α]]): M[Unit] =
    selTrigger[D1 :: D2 :: HNil](Sel(ref1, ref2))(λ[Id ~> λ[α => (D1 :: D2 :: HNil) => TriggerF[M, α]]](
      α => l => f(α)(l.head, l.tail.head)
    ))

  def selThreshold2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: (D1, D2) => Option[M[Unit]]): M[Unit] =
    selTrigger2[D1, D2](ref1, ref2)(λ[Id ~> λ[α => (D1, D2) => TriggerF[M, α]]](α => (d1, d2) => f(d1, d2) match {
      case None => TriggerF.Sleep(α)
      case Some(mu) => TriggerF.Fire(mu)
    }))

  def peek[D](ref: Ref[D])(f: D => M[Unit])(implicit dom: Dom[D]): M[Unit] =
    observe(ref).by(d => fire(f(d)))

  def alternate[D1, D2, L, R](ref1: Ref[D1], ref2: Ref[D2])(
    f: (D1, D2) => Alternator,
    onStartLeft: () => M[L],
    onStartRight: () => M[R],
    onSwitchToLeft: R => M[L],
    onSwitchToRight: L => M[R],
    onStop: Option[Either[L, R]] => M[Unit]
  )(implicit
    dom1: Dom[D1],
    dom2: Dom[D2],
    M: Bind[M]
  ): M[Unit] = {
    import TriggerF._
    def observeLeft(d2: D2, l: L): M[Unit] = observe(ref1).by(λ[Id ~> λ[α => D1 => TriggerF[M, α]]](α => d1 => f(d1, d2) match {
      case Alternator.Left  => Sleep(α)
      case Alternator.Right => Fire(onSwitchToRight(l) >>= { observeRight(d1, _) })
      case Alternator.Stop  => Fire(onStop(Some(Left(l))))
    }))
    def observeRight(d1: D1, r: R): M[Unit] = observe(ref2).by(λ[Id ~> λ[α => D2 => TriggerF[M, α]]](α => d2 => f(d1, d2) match {
      case Alternator.Left  => Fire(onSwitchToLeft(r) >>= { observeLeft(d2, _) })
      case Alternator.Right => Sleep(α)
      case Alternator.Stop  => Fire(onStop(Some(Right(r))))
    }))
    peek(ref1)(d1 => {
      peek(ref2)(d2 => {
        f(d1, d2) match {
          case Alternator.Left  => onStartLeft() >>= { observeLeft(d2, _) }
          case Alternator.Right => onStartRight() >>= { observeRight(d1, _) }
          case Alternator.Stop  => onStop(None)
        }
      })
    })
  }

  def alternate0[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(
    f: (D1, D2) => Alternator,
    onSwitchToLeft: M[Unit],
    onSwitchToRight: M[Unit],
    onStop: M[Unit]
  )(implicit
    dom1: Dom[D1],
    dom2: Dom[D2],
    M: Bind[M]
  ): M[Unit] =
    alternate[D1, D2, Unit, Unit](ref1, ref2)(
      f,
      () => onSwitchToLeft,
      () => onSwitchToRight,
      (_ => onSwitchToLeft),
      (_ => onSwitchToRight),
      (_ => onStop)
    )
}

object Propagation {
  def apply[M[_], Ref[_]](implicit M: Propagation[M, Ref]): Propagation[M, Ref] = M

  val module: PersistentPropagationModule = PropagationImpl
  val bundle: PropagationBundle = PropagationImpl
}

