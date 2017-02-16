package nutcracker

import scala.language.higherKinds
import scalaz.{Applicative, Bind, Traverse, |>=|, ~>}
import scalaz.std.vector._
import scalaz.syntax.bind._
import shapeless.{::, HList, HNil}
import Trigger._
import monocle.Lens
import nutcracker.util.{FreeK, FunctorKA, HEqualK, InjectK, ShowK, StateInterpreter}

import scalaz.Id.Id

trait Propagation[M[_], Ref[_]] extends PSrc[Ref, M] {

  // basic instructions

  def newCell[D](d: D)(implicit dom: Dom[D]): M[Ref[D]]

  def updateImpl[D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): M[Unit]

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: Id ~> λ[α => L => TriggerF[M, α]]): M[Unit]


  def update[D](ref: Ref[D])(implicit dom: Dom[D]): UpdateSyntaxHelper[D, dom.Update, dom.Delta] =
    new UpdateSyntaxHelper[D, dom.Update, dom.Delta](ref)(dom)

  final class UpdateSyntaxHelper[D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {
    def by(u: U): M[Unit] = updateImpl(ref)(u)
  }


  // derived methods

  def cells[D](d: D, n: Int)(implicit dom: Dom[D], M: Applicative[M]): M[Vector[Ref[D]]] =
    Traverse[Vector].sequence(Vector.fill(n)(newCell(d)))

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

  trait Module {
    type Ref[_]
    type Lang[K[_], A]
    type State[K]

    implicit def refEquality: HEqualK[Ref]
    implicit def refShow: ShowK[Ref]
    implicit def functorKAPropLang: FunctorKA[Lang]
    implicit def propagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref]

    def empty[K]: State[K]
    def emptyF[F[_[_], _]]: State[FreeK[F, Unit]]
    def interpreter: StateInterpreter[Lang, State]
    def dfsSolver: DFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]]]

    def naiveAssess[K[_], S[_]](
      lens: Lens[S[K[Unit]], State[K[Unit]]])(implicit
      ord: K |>=| FreeK[Lang, ?]
    ): S[K[Unit]] => Assessment[List[K[Unit]]]

    def fetch[K, D](s: State[K])(ref: Ref[D]): D
    def fetchResult[K, D](s: State[K])(ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out]
  }

  val module: Module = PropagationModuleImpl
}