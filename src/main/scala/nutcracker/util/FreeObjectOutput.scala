package nutcracker.util

import nutcracker.util.free.Free

import scalaz.Id.Id
import scalaz.{-\/, Monoid, Writer, \/, \/-, ~>}
import scalaz.syntax.monad._

final class FreeObjectOutput[R, Ptr[_], A] private[FreeObjectOutput] (private val unwrap: Free[FreeObjectOutput.OutputInst[R, Ptr, ?], A]) /* extends AnyVal // can't have nested AnyVals :( */ {
  import FreeObjectOutput._

  def flatMap[B](f: A => FreeObjectOutput[R, Ptr, B]): FreeObjectOutput[R, Ptr, B] =
    wrap(unwrap flatMap (a => f(a).unwrap))

  private def foldMap(showRef: Ptr ~> λ[α => R]): Lst[R] =
    unwrap.foldMap[Writer[Lst[R], ?]](λ[OutputInst[R, Ptr, ?] ~> Writer[Lst[R], ?]](_ match {
      case Write(r) => Writer(Lst.singleton(r), ())
      case WriteObject(pa, _) => Writer(Lst.singleton(showRef(pa)), ())
    })).run._1

  /**
    *
    * @param s1 initial state
    * @param step given the accumulted state on the way down (`S1`) and a pointer (`Ptr[α]`),
    *             `step` has to decide whether
    *              - follow the pointer down, in which case it returns `Left` with new `S1` to
    *                be passed downwards and a function that
    *                 - transforms the state passed upwards (`S2`) and
    *                 - produces decoration for the result of recursion; or
    *              - the pointer can be handled directly, in which case it produces serialized
    *                representation of the pointer (`R`) and state to be passed upwards (`S2`).
    * @param S2 used to combine state passed up from siblings to state passed up by their parent.
    * @tparam S1 state to be passed downwards
    * @tparam S2 state to be passed upwards
    */
  private def foldBiState[S1, S2](s1: S1)(step: S1 => Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)])(deref: Ptr ~> Id)(implicit S2: Monoid[S2]): Lst[R] = {
    type S2_ = (S2, Lst[R]); import scalaz.std.tuple.tuple2Monoid
    unwrap.foldRunRecParM[Id, S1, S2_](s1, λ[λ[α => (S1, OutputInst[R, Ptr, α])] ~> λ[α => (S1, Free[OutputInst[R, Ptr, ?], α], S2_ => S2_) \/ (S2_, α)]] {
      case (s1, inst) => inst match {
        case Write(r) => \/-(((S2.zero, Lst.singleton(r)), ()))
        case WriteObject(pa, ser) => step(s1)(pa) match {
          case -\/((s11, tr)) => -\/((s11, ser.write(FreeObjectOutput.empty[R, Ptr], deref(pa)).unwrap, { case (s2, r) => tr(s2) match { case (s2, decor) => (s2, decor.decorate(r)(_ +: _, _ :+ _)) }}))
          case \/-((s2, r)) => \/-(((s2, Lst.singleton(r)), ()))
        }
      }
    })._1._2
  }

  /** Specialized [[foldBiState]] for `S2 = Unit`. */
  private def foldState[S](s: S)(step: S => Ptr ~> λ[α => (S, Decoration[R]) \/ R])(deref: Ptr ~> Id): Lst[R] = {
    type S2 = Lst[R]
    unwrap.foldRunRecParM[Id, S, S2](s, λ[λ[α => (S, OutputInst[R, Ptr, α])] ~> λ[α => (S, Free[OutputInst[R, Ptr, ?], α], S2 => S2) \/ (S2, α)]] {
      case (s, inst) => inst match {
        case Write(r) => \/-((Lst.singleton(r), ()))
        case WriteObject(pa, ser) => step(s)(pa) match {
          case -\/((s1, decor)) => -\/((s1, ser.write(FreeObjectOutput.empty[R, Ptr], deref(pa)).unwrap, r => decor.decorate(r)(_ +: _, _ :+ _)))
          case \/-(r) => \/-((Lst.singleton(r), ()))
        }
      }
    })._1
  }

  def show(showReference: Ptr ~> λ[α => R]): Lst[R] =
    foldMap(showReference)

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def show(
    deref: Ptr ~> Id,
    decorateReferenced: Ptr ~> λ[α => Decoration[R]],
    decorateUnreferenced: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr]
  ): Lst[R] = {
    type S1 = List[Ptr[_]] // context, i.e. all parents
    type S2 = Lst[Ptr[_]]  // referenced within the subgraph

    val step = (s1: S1) =>
      λ[Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)]](pa => {
        if(s1.exists(E.hEqual(_, pa))) \/-((Lst.singleton(pa), showReference(pa)))
        else -\/((pa :: s1, s2 => {
          val s21 = s2.filterNot(E.hEqual(_, pa))
          val dec = if(s21.size < s2.size) decorateReferenced(pa) else decorateUnreferenced(pa)
          (s21, dec)
        }))
      })

    foldBiState[S1, S2](Nil)(step)(deref)
  }

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def show(
    deref: Ptr ~> Id,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr]
  ): Lst[R] = {
    type S = List[Ptr[_]] // context, i.e. all parents

    val step = (s: S) =>
      λ[Ptr ~> λ[α => (S, Decoration[R]) \/ R]](pa => {
        if(s.exists(E.hEqual(_, pa))) \/-(showReference(pa))
        else -\/((pa :: s, decorateContent(pa)))
      })

    foldState[S](Nil)(step)(deref)
  }
}

object FreeObjectOutput {
  private[util] sealed trait OutputInst[R, Ptr[_], A]
  private[FreeObjectOutput] case class Write[R, Ptr[_]](r: R) extends OutputInst[R, Ptr, Unit]
  private[FreeObjectOutput] case class WriteObject[R, Ptr[_], A](pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]) extends OutputInst[R, Ptr, Unit]

  private def wrap[R, Ptr[_], A](fa: Free[FreeObjectOutput.OutputInst[R, Ptr, ?], A]): FreeObjectOutput[R, Ptr, A] = wrap[R, Ptr, A](fa)

  def point[R, Ptr[_], A](a: A): FreeObjectOutput[R, Ptr, A] = wrap(Free.point[OutputInst[R, Ptr, ?], A](a))
  def empty[R, Ptr[_]]: FreeObjectOutput[R, Ptr, Unit] = point(())
  def write[R, Ptr[_]](r: R): FreeObjectOutput[R, Ptr, Unit] = wrap(Free.liftF[OutputInst[R, Ptr, ?], Unit](Write(r)))
  def writeObject[R, Ptr[_], A](pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] = wrap(Free.liftF[OutputInst[R, Ptr, ?], Unit](WriteObject(pa, ser)))

  sealed abstract class Decoration[+R] {
    def beforeOption: Option[R]
    def afterOption: Option[R]
    def decorate[S](s: S)(prepend: (R, S) => S, append: (S, R) => S): S = this match {
      case Naked => s
      case BeforeAfter(r1, r2) => append(prepend(r1, s), r2)
      case Before(r1) => prepend(r1, s)
      case After(r2) => append(s, r2)
    }
  }
  case object Naked extends Decoration[Nothing] { def beforeOption = None; def afterOption = None }
  case class Before[R](r: R) extends Decoration[R] { def beforeOption = Some(r); def afterOption = None }
  case class After[R](r: R) extends Decoration[R] { def beforeOption = None; def afterOption = Some(r) }
  case class BeforeAfter[R](before: R, after: R) extends Decoration[R] { def beforeOption = Some(before); def afterOption = Some(after) }

  implicit def objectOutputInstance[R, Ptr[_]]: ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] =
    new ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] {

      def write(out: FreeObjectOutput[R, Ptr, Unit], r: R): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.write(r).unwrap)

      def writeObject[A](out: FreeObjectOutput[R, Ptr, Unit], pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.writeObject(pa, ser).unwrap)
    }

  implicit def monadObjectOutputInstance[R, Ptr[_]]: MonadObjectOutput[FreeObjectOutput[R, Ptr, ?], R, Ptr] =
    new MonadObjectOutput[FreeObjectOutput[R, Ptr, ?], R, Ptr] {

      def writeObject[A](pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
        FreeObjectOutput.writeObject(pa, ser)

      def writer[A](w: R, v: A): FreeObjectOutput[R, Ptr, A] =
        wrap(FreeObjectOutput.write[R, Ptr](w).unwrap >> FreeObjectOutput.point(v).unwrap)

      def bind[A, B](fa: FreeObjectOutput[R, Ptr, A])(f: A => FreeObjectOutput[R, Ptr, B]): FreeObjectOutput[R, Ptr, B] =
        fa flatMap f

      def point[A](a: => A): FreeObjectOutput[R, Ptr, A] =
        FreeObjectOutput.point(a)
    }
}