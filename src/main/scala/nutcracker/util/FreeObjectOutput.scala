package nutcracker.util

import nutcracker.util.free.Free
import nutcracker.util.ops.toFoldableOps

import scalaz.Id.Id
import scalaz.{-\/, BindRec, Monoid, Writer, \/, \/-, ~>}
import scalaz.Leibniz.===
import scalaz.syntax.monad._

final class FreeObjectOutput[R, Ptr[_], A] private[FreeObjectOutput] (private val unwrap: Free[FreeObjectOutput.OutputInst[R, Ptr, ?], A]) /* extends AnyVal // can't have nested AnyVals :( */ {
  import FreeObjectOutput._

  def flatMap[B](f: A => FreeObjectOutput[R, Ptr, B]): FreeObjectOutput[R, Ptr, B] =
    wrap(unwrap flatMap (a => f(a).unwrap))

  def ++[B](that: FreeObjectOutput[R, Ptr, B])(implicit ev: A =:= Unit): FreeObjectOutput[R, Ptr, B] = this >> that
  def :+(r: R)(implicit ev: A =:= Unit): FreeObjectOutput[R, Ptr, Unit] = this ++ write(r)
  def +:(r: R): FreeObjectOutput[R, Ptr, A] = FreeObjectOutput.write(r) ++ this

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

  def eval(showReference: Ptr ~> λ[α => R]): Lst[R] =
    foldMap(showReference)

  def appendTo[B](b: B, showReference: Ptr ~> λ[α => R])(implicit agg: Aggregator[B, R]): B =
    eval(showReference).aggregateLeft(b)

  def showShallow(showReference: Ptr ~> λ[α => R])(implicit agg: Aggregator[StringBuilder, R]): String =
    appendTo(new StringBuilder, showReference).result()

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def eval(
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

  def appendTo[B](
    b: B,
    deref: Ptr ~> Id,
    decorateReferenced: Ptr ~> λ[α => Decoration[R]],
    decorateUnreferenced: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[B, R],
    E: HEqualK[Ptr]
  ): B =
    eval(deref, decorateReferenced, decorateUnreferenced, showReference).aggregateLeft(b)

  def showAutoLabeled(deref: Ptr ~> Id, showRef: Ptr ~> λ[α => String])(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration(s"<def ${showRef(ref)}>", "</def>")),
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration("", "")),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit
    E: HEqualK[Ptr],
    ev: R === String
  ): String =
    ev.subst[FreeObjectOutput[?, Ptr, A]](this).appendTo(
      new StringBuilder,
      deref,
      decorateReferenced,
      decorateUnreferenced,
      λ[Ptr ~> λ[α => String]](p => decorateReference(showRef(p)))
    ).result()

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def eval(
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

  def appendTo[B](
    b: B,
    deref: Ptr ~> Id,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[B, R],
    E: HEqualK[Ptr]
  ): B =
    eval(deref, decorateContent, showReference).aggregateLeft(b)

  def showLabeled(
    deref: Ptr ~> Id,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[StringBuilder, R],
    E: HEqualK[Ptr]
  ): String =
    appendTo(new StringBuilder, deref, decorateContent, showReference).result()

  def writeTo[O](out: O)(implicit O: ObjectOutput[O, R, Ptr]): O =
    unwrap.foldRun[O](out, λ[λ[α => (O, OutputInst[R, Ptr, α])] ~> (O, ?)] {
      case (out, i) => i match {
        case Write(r) => (O.write(out, r), ())
        case WriteObject(pa, ser) => (O.writeObject(out, pa)(ser), ())
      }
    })._1

  def serialize[M[_]](implicit M: MonadObjectOutput[M, R, Ptr], M1: BindRec[M]): M[A] =
    unwrap.foldMap[M](λ[OutputInst[R, Ptr, ?] ~> M](_ match {
      case Write(r) => M.write(r)
      case WriteObject(pa, ser) => M.writeObject(pa)(ser)
    }))
}

object FreeObjectOutput {
  private[util] sealed trait OutputInst[R, Ptr[_], A]
  private[FreeObjectOutput] case class Write[R, Ptr[_]](r: R) extends OutputInst[R, Ptr, Unit]
  private[FreeObjectOutput] case class WriteObject[R, Ptr[_], A](pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]) extends OutputInst[R, Ptr, Unit]

  private def wrap[R, Ptr[_], A](fa: Free[FreeObjectOutput.OutputInst[R, Ptr, ?], A]): FreeObjectOutput[R, Ptr, A] = new FreeObjectOutput(fa)

  def point[R, Ptr[_], A](a: A): FreeObjectOutput[R, Ptr, A] = wrap(Free.point[OutputInst[R, Ptr, ?], A](a))
  def empty[R, Ptr[_]]: FreeObjectOutput[R, Ptr, Unit] = point(())
  def write[R, Ptr[_]](r: R): FreeObjectOutput[R, Ptr, Unit] = wrap(Free.liftF[OutputInst[R, Ptr, ?], Unit](Write(r)))
  def writeObject[R, Ptr[_], A](pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] = wrap(Free.liftF[OutputInst[R, Ptr, ?], Unit](WriteObject(pa, ser)))

  sealed abstract class IndexedDecoration[+R1, +R2] {
    def beforeOption: Option[R1]
    def afterOption: Option[R2]
    def decorate[S](s: S)(prepend: (R1, S) => S, append: (S, R2) => S): S = this match {
      case Naked => s
      case BeforeAfter(r1, r2) => append(prepend(r1, s), r2)
      case Before(r1) => prepend(r1, s)
      case After(r2) => append(s, r2)
    }
  }
  case object Naked extends IndexedDecoration[Nothing, Nothing] { def beforeOption = None; def afterOption = None }
  case class Before[R1](r1: R1) extends IndexedDecoration[R1, Nothing] { def beforeOption = Some(r1); def afterOption = None }
  case class After[R2](r2: R2) extends IndexedDecoration[Nothing, R2] { def beforeOption = None; def afterOption = Some(r2) }
  case class BeforeAfter[R1, R2](before: R1, after: R2) extends IndexedDecoration[R1, R2] { def beforeOption = Some(before); def afterOption = Some(after) }
  object IndexedDecoration {
    def naked: IndexedDecoration[Nothing, Nothing] = Naked
    def apply[R1, R2](l: R1, r: R2): IndexedDecoration[R1, R2] = BeforeAfter(l, r)
    def before[R1](before: R1): IndexedDecoration[R1, Nothing] = Before(before)
    def after[R2](after: R2): IndexedDecoration[Nothing, R2] = After(after)
  }
  type Decoration[+R] = IndexedDecoration[R, R]
  object Decoration {
    def naked: Decoration[Nothing] = Naked
    def apply[R](l: R, r: R): Decoration[R] = BeforeAfter(l, r)
    def before[R](before: R): Decoration[R] = Before(before)
    def after[R](after: R): Decoration[R] = After(after)
  }

  implicit def objectOutputInstance[R, Ptr[_]]: ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] =
    new ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] {

      def write(out: FreeObjectOutput[R, Ptr, Unit], r: R): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.write(r).unwrap)

      def writeObject[A](out: FreeObjectOutput[R, Ptr, Unit], pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.writeObject(pa, ser).unwrap)
    }

  implicit def monadObjectOutputInstance[R, Ptr[_]]: MonadObjectOutput[FreeObjectOutput[R, Ptr, ?], R, Ptr] with BindRec[FreeObjectOutput[R, Ptr, ?]] =
    new MonadObjectOutput[FreeObjectOutput[R, Ptr, ?], R, Ptr] with BindRec[FreeObjectOutput[R, Ptr, ?]] {

      def writeObject[A](pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
        FreeObjectOutput.writeObject(pa, ser)

      def writer[A](w: R, v: A): FreeObjectOutput[R, Ptr, A] =
        wrap(FreeObjectOutput.write[R, Ptr](w).unwrap >> FreeObjectOutput.point(v).unwrap)

      def bind[A, B](fa: FreeObjectOutput[R, Ptr, A])(f: A => FreeObjectOutput[R, Ptr, B]): FreeObjectOutput[R, Ptr, B] =
        fa flatMap f

      def point[A](a: => A): FreeObjectOutput[R, Ptr, A] =
        FreeObjectOutput.point(a)

      def tailrecM[A, B](a: A)(f: A => FreeObjectOutput[R, Ptr, A \/ B]): FreeObjectOutput[R, Ptr, B] =
        bind(f(a))(_ match {
          case -\/(a) => tailrecM(a)(f)
          case \/-(b) => point(b)
        })
    }

  implicit def monoidInstance[R, Ptr[_]]: Monoid[FreeObjectOutput[R, Ptr, Unit]] = new Monoid[FreeObjectOutput[R, Ptr, Unit]] {
    def append(f1: FreeObjectOutput[R, Ptr, Unit], f2: => FreeObjectOutput[R, Ptr, Unit]): FreeObjectOutput[R, Ptr, Unit] = f1 ++ f2
    def zero: FreeObjectOutput[R, Ptr, Unit] = empty[R, Ptr]
  }

  implicit def aggregatorInstance[R, Ptr[_]]: Aggregator[FreeObjectOutput[R, Ptr, Unit], R] =
    Aggregator(_ :+ _)
}