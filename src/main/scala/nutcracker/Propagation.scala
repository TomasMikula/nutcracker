package nutcracker

import scala.language.higherKinds
import scalaz.{Applicative, Bind, Traverse}
import scalaz.std.vector._
import scalaz.syntax.bind._
import shapeless.{::, HList, HNil}

trait Propagation[M[_], Ref[_]] extends PSrc[Ref, M] {

  // basic instructions

  def cell[D](d: D)(implicit dom: Dom[D]): M[Ref[D]]

  def updateImpl[D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): M[Unit]

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: L => Trigger[M[Unit]]): M[Unit]


  def update[D](ref: Ref[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl(ref)(u)
  }


  // derived methods

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    Traverse[Vector].sequence(Vector.fill(n)(cell(d)))

  def valTrigger[D](ref: Ref[D])(f: D => Trigger[M[Unit]])(implicit dom: Dom[D]): M[Unit] =
    observe(ref).by(d => f(d) match {
      case FireReload(k) => (Some(k), Some((d, δ) => f(d)))
      case Fire(k) => (Some(k), None)
      case Sleep() => (None, Some((d, δ) => f(d)))
      case Discard() => (None, None)
    })

  def selTrigger2[D1, D2](ref1: Ref[D1], ref2: Ref[D2])(f: (D1, D2) => Trigger[M[Unit]]): M[Unit] =
    selTrigger[D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))

  def peek[D](ref: Ref[D])(f: D => M[Unit])(implicit dom: Dom[D]): M[Unit] =
    valTrigger(ref)(d => Fire(f(d)))

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
    def observeLeft(d2: D2, l: L): M[Unit] = valTrigger(ref1)(d1 => f(d1, d2) match {
      case Alternator.Left  => Sleep()
      case Alternator.Right => Fire(onSwitchToRight(l) >>= { observeRight(d1, _) })
      case Alternator.Stop  => Fire(onStop(Some(Left(l))))
    })
    def observeRight(d1: D1, r: R): M[Unit] = valTrigger(ref2)(d2 => f(d1, d2) match {
      case Alternator.Left  => Fire(onSwitchToLeft(r) >>= { observeLeft(d2, _) })
      case Alternator.Right => Sleep()
      case Alternator.Stop  => Fire(onStop(Some(Right(r))))
    })
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
}