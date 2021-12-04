package nutcracker.util

import nutcracker.util.free.Free
import nutcracker.util.ops.Ops.toFoldableOps

import scala.annotation.tailrec
import scalaz.{-\/, Applicative, BindRec, Foldable, Monoid, Writer, \/, \/-, ~>, ===}
import scalaz.syntax.monad._

final class FreeObjectOutput[R, Ptr[_], A] private[FreeObjectOutput] (private val unwrap: Free[FreeObjectOutput.OutputInst[R, Ptr, *], A]) /* extends AnyVal // can't have nested AnyVals :( */ {
  import FreeObjectOutput._

  def flatMap[B](f: A => FreeObjectOutput[R, Ptr, B]): FreeObjectOutput[R, Ptr, B] =
    wrap(unwrap flatMap (a => f(a).unwrap))

  def ++[B](that: FreeObjectOutput[R, Ptr, B])(implicit ev: A =:= Unit): FreeObjectOutput[R, Ptr, B] = this >> that
  def :::(that: FreeObjectOutput[R, Ptr, Unit]): FreeObjectOutput[R, Ptr, A] = that ++ this
  def :+(r: R)(implicit ev: A =:= Unit): FreeObjectOutput[R, Ptr, Unit] = this ++ write(r)
  def +:(r: R): FreeObjectOutput[R, Ptr, A] = write(r) ++ this
  def ::(r: R): FreeObjectOutput[R, Ptr, A] = r +: this

  private def foldMap(showRef: Ptr ~> λ[α => R]): Lst[R] =
    unwrap.foldMapRec[Writer[Lst[R], *]](
      new (OutputInst[R, Ptr, *] ~> λ[α => Writer[Lst[R], Free[OutputInst[R, Ptr, *], α] \/ α]]) {
        override def apply[X](fa: OutputInst[R, Ptr, X]) =
          fa match {
            case Write(r) => Writer(Lst.singleton(r), \/-(()))
            case WriteRec(pa, _) => Writer(Lst.singleton(showRef(pa)), \/-(()))
            case Nest(fa) => Writer(Lst.empty[R], -\/(fa.unwrap))
          }
      }
    ).run._1

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
  private def foldBiState[S1, S2, M[_]](s1: S1)(step: S1 => Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)])(deref: Ptr ~> M)(implicit S2: Monoid[S2], M0: BindRec[M], M1: Applicative[M]): M[Tree[R]] = {
    import Tree._
    type S2_ = (S2, Tree[R]); import scalaz.std.tuple.tuple2Monoid
    M0.map(
      unwrap.foldRunRecParM[M, S1, S2_](
        s1,
        new (λ[α => (S1, OutputInst[R, Ptr, α])] ~> λ[α => M[(S1, Free[OutputInst[R, Ptr, *], α], S2_ => S2_) \/ (S2_, α)]]) {
          override def apply[X](fx: (S1, OutputInst[R, Ptr, X])) =
            fx match {
              case (s1, inst) => inst match {
                case Write(r) => M1.point(\/-(((S2.zero, singleton(r)), ())))
                case WriteRec(pa, f) => step(s1)(pa) match {
                  case -\/((s11, tr)) => M0.map(deref(pa)) { a =>
                    -\/((s11, f(a).unwrap, {
                      case (s2, tree) => tr(s2) match { case (s2, decor) => (s2, nest(decor.decorate(tree)(_ +: _, _ :+ _))) }
                    }))
                  }
                  case \/-((s2, r)) => M1.point(\/-(((s2, singleton(r)), ())))
                }
                case Nest(fx) => M1.point(-\/((s1, fx.unwrap, { case (s2, tree) => (s2, nest(tree)) })))
              }
            }
        }
      )
    )(_._1._2)
  }

  /** Specialized [[foldBiState]] for `S2 = Unit`. */
  private def foldState[S, M[_]](s: S)(step: S => Ptr ~> λ[α => (S, Decoration[R]) \/ R])(deref: Ptr ~> M)(implicit M0: BindRec[M], M1: Applicative[M]): M[Tree[R]] = {
    type S2 = Tree[R]
    M0.map(
      unwrap.foldRunRecParM[M, S, S2](
        s,
        new (λ[α => (S, OutputInst[R, Ptr, α])] ~> λ[α => M[(S, Free[OutputInst[R, Ptr, *], α], S2 => S2) \/ (S2, α)]]) {
          override def apply[X](fx: (S, OutputInst[R, Ptr, X])) =
            fx match {
              case (s, inst) => inst match {
                case Write(r) => M1.point(\/-((Tree.singleton(r), ())))
                case WriteRec(pa, f) => step(s)(pa) match {
                  case -\/((s1, decor)) => M0.map(deref(pa))(a => -\/((s1, f(a).unwrap, t => decor.decorate(t)(_ +: _, _ :+ _))))
                  case \/-(r) => M1.point(\/-((Tree.singleton(r), ())))
                }
                case Nest(fx) => M1.point(-\/((s, fx.unwrap, tree => Tree.nest(tree))))
              }
            }
        },
      )
    )(_._1)
  }

  def eval(showReference: Ptr ~> λ[α => R]): Lst[R] =
    foldMap(showReference)

  def appendTo[B](b: B, showReference: Ptr ~> λ[α => R])(implicit agg: Aggregator[B, R]): B =
    eval(showReference).aggregateLeft(b)

  def showShallow(showReference: Ptr ~> λ[α => R])(implicit agg: Aggregator[StringBuilder, R]): String =
    appendTo(new StringBuilder, showReference).result()

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def eval[M[_]](
    deref: Ptr ~> M,
    decorateReferenced: Ptr ~> λ[α => Decoration[R]],
    decorateUnreferenced: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M]
  ): M[Tree[R]] = {
    type S1 = List[Exists[Ptr]] // context, i.e. all parents
    type S2 = Lst[Exists[Ptr]]  // referenced within the subgraph

    val step = (s1: S1) =>
      new (Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)]) {
        override def apply[X](px: Ptr[X]) = {
          if(s1.exists(p => E.equal(p.value, px))) \/-((Lst.singleton(Exists(px)), showReference(px)))
          else -\/((Exists(px) :: s1, s2 => {
            val s21 = s2.filterNot(p => E.equal(p.value, px))
            val dec = if(s21.size < s2.size) decorateReferenced(px) else decorateUnreferenced(px)
            (s21, dec)
          }))
        }
      }

    foldBiState[S1, S2, M](Nil)(step)(deref)
  }

  def appendTo[B, M[_]](
    b: B,
    deref: Ptr ~> M,
    decorateReferenced: Ptr ~> λ[α => Decoration[R]],
    decorateUnreferenced: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[B, R],
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M]
  ): M[B] =
    M0.map(eval(deref, decorateReferenced, decorateUnreferenced, showReference))(_.aggregateLeft(b))

  def showAutoLabeled[M[_]](deref: Ptr ~> M, showRef: Ptr ~> λ[α => String])(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] =
      new (Ptr ~> λ[α => Decoration[String]]) { override def apply[X](ref: Ptr[X]) = Decoration(s"<def ${showRef(ref)}>", "</def>") },
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] =
      new (Ptr ~> λ[α => Decoration[String]]) { override def apply[X](ref: Ptr[X]) = Decoration.naked },
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M],
    ev: R === String
  ): M[String] =
    M0.map(ev.subst[FreeObjectOutput[*, Ptr, A]](this).appendTo(
      new StringBuilder,
      deref,
      decorateReferenced,
      decorateUnreferenced,
      new (Ptr ~> λ[α => String]) { override def apply[X](p: Ptr[X]) = decorateReference(showRef(p)) }
    ))(_.result())

  def printTree[M[_]](deref: Ptr ~> M, showRef: Ptr ~> λ[α => String], lineLimit: Int = 60, tab: String = "  ", newLine: String = "\n")(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] =
      new (Ptr ~> λ[α => Decoration[String]]) {
        override def apply[X](ref: Ptr[X]) = Decoration(s"<def ${showRef(ref)}>", "</def>")
      },
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] =
      new (Ptr ~> λ[α => Decoration[String]]) {
        override def apply[X](ref: Ptr[X]) = Decoration.naked
      },
    decorateReference: String => String =
      ref => s"<ref $ref/>"
  )(implicit
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M],
    ev: R === String
  ): M[String] =
    M0.map(ev.subst[FreeObjectOutput[*, Ptr, A]](this).eval(
      deref,
      decorateReferenced,
      decorateUnreferenced,
      new (Ptr ~> λ[α => String]) {
        override def apply[X](p: Ptr[X]) = decorateReference(showRef(p))
      },
    ))(_.print(lineLimit, tab, newLine)(_.length).aggregateLeft(new StringBuilder).result())

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showReference`. */
  def eval[M[_]](
    deref: Ptr ~> M,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M]
  ): M[Tree[R]] = {
    type S = List[Exists[Ptr]] // context, i.e. all parents

    val step = (s: S) =>
      new (Ptr ~> λ[α => (S, Decoration[R]) \/ R]) {
        override def apply[X](px: Ptr[X]) =
          if (s.exists(p => E.equal(p.value, px))) \/-(showReference(px))
          else -\/((Exists(px) :: s, decorateContent(px)))
      }

    foldState[S, M](Nil)(step)(deref)
  }

  def appendTo[B, M[_]](
    b: B,
    deref: Ptr ~> M,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[B, R],
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M]
  ): M[B] =
    M0.map(eval(deref, decorateContent, showReference))(_.aggregateLeft(b))

  def showLabeled[M[_]](
    deref: Ptr ~> M,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    agg: Aggregator[StringBuilder, R],
    E: HEqualK[Ptr],
    M0: BindRec[M],
    M1: Applicative[M]
  ): M[String] =
    M0.map(appendTo(new StringBuilder, deref, decorateContent, showReference))(_.result())

  def writeTo[O](out: O)(implicit O: ObjectOutput[O, R, Ptr]): O =
    unwrap.foldRunRec[O](
      out,
      new (λ[α => (O, OutputInst[R, Ptr, α])] ~> λ[α => (O, Free[OutputInst[R, Ptr, *], α], O => O) \/ (O, α)]) {
        override def apply[X](fx: (O, OutputInst[R, Ptr, X])) = fx match {
          case (out, i) => i match {
            case Write(r) => \/-((O.write(out, r), ()))
            case WriteRec(pa, f) => \/-((O.writeSubObject(out, pa)(f), ()))
            case Nest(fx) => -\/((out, fx.unwrap, o => o)) // Note: if ObjectOutput supported nesting, here we would
                                                           // "open a parenthesis" and close it in the continuation
          }
        }
      }
    )._1

  def serialize[M[_]](implicit M: MonadObjectOutput[M, R, Ptr]): M[A] =
    unwrap.foldMap[M](
      new (OutputInst[R, Ptr, *] ~> M) {
        override def apply[X](fx: OutputInst[R, Ptr, X]) =
          fx match {
            case Write(r) => M.write(r)
            case WriteRec(pa, f) => M.writeRec(pa)(f(_).serialize[M])
            case Nest(fx) => M.nest(fx.serialize[M])
          }
      }
    )
}

object FreeObjectOutput {
  private[util] sealed trait OutputInst[R, Ptr[_], A]
  private[FreeObjectOutput] case class Write[R, Ptr[_]](r: R) extends OutputInst[R, Ptr, Unit]
  private[FreeObjectOutput] case class WriteRec[R, Ptr[_], A](pa: Ptr[A], f: A => FreeObjectOutput[R, Ptr, Unit]) extends OutputInst[R, Ptr, Unit]
  private[FreeObjectOutput] case class Nest[R, Ptr[_], A](fa: FreeObjectOutput[R, Ptr, A]) extends OutputInst[R, Ptr, A]

  private def wrap[R, Ptr[_], A](fa: Free[FreeObjectOutput.OutputInst[R, Ptr, *], A]): FreeObjectOutput[R, Ptr, A] = new FreeObjectOutput(fa)

  def point[R, Ptr[_], A](a: A): FreeObjectOutput[R, Ptr, A] = wrap(Free.point[OutputInst[R, Ptr, *], A](a))
  def empty[R, Ptr[_]]: FreeObjectOutput[R, Ptr, Unit] = point(())
  def write[R, Ptr[_]](r: R): FreeObjectOutput[R, Ptr, Unit] = wrap(Free.liftF[OutputInst[R, Ptr, *], Unit](Write(r)))
  def writeRec[R, Ptr[_], A](pa: Ptr[A], f: A => FreeObjectOutput[R, Ptr, Unit]): FreeObjectOutput[R, Ptr, Unit] =
    wrap(Free.liftF[OutputInst[R, Ptr, *], Unit](WriteRec(pa, f)))
  def writeObject[R, Ptr[_], A](pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
    writeRec(pa, (a: A) => ser.free(a))
  def nest[R, Ptr[_], A](fa: FreeObjectOutput[R, Ptr, A]): FreeObjectOutput[R, Ptr, A] = wrap(Free.liftF[OutputInst[R, Ptr, *], A](Nest(fa)))

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

  final case class Tree[R](lst: Lst[R \/ Tree[R]]) { // extends AnyVal { // https://issues.scala-lang.org/browse/SI-9600
    def ++(that: Tree[R]): Tree[R] = Tree(this.lst ++ that.lst)
    def :+(r: R): Tree[R] = Tree(lst :+ -\/(r))
    def +:(r: R): Tree[R] = Tree(-\/[R, Tree[R]](r) +: lst)

    def foldLeft[B](z: B)(f: (B, R) => B): B = {
      @tailrec def go(z: B, lst: Lst[R \/ Tree[R]]): B = lst.uncons match {
        case Some((rt, lst)) => rt match {
          case -\/(r) => go(f(z, r), lst)
          case \/-(t) => go(z, t.lst ++ lst)
        }
        case None => z
      }

      go(z, lst)
    }

    def print(lineLimit: Int, tab: R, newLine: R)(implicit measure: R => Int): Lst[R] =
      print(0, lineLimit).uncons match {
        case None => Lst.empty
        case Some((l, ls)) => printLine(l, tab) ::: ls.foldRight(Lst.empty[R])((ln, rs) => newLine :: printLine(ln, tab) ::: rs)
      }

    type Line = (/* indent: */ Int, /* tokens: */ Lst[R])

    private def print(indent: Int, lineLimit: Int)(implicit measure: R => Int): Lst[Line] = {
      if(fitsOnALine(lineLimit)) Lst.singleton((indent, flatten))
      else lst.flatMap(_ match {
        case -\/(r) => Lst.singleton((indent, Lst.singleton(r)))
        case \/-(tree) => tree.print(indent + 1, lineLimit)
      })
    }

    private def printLine(line: Line, tab: R): Lst[R] = {
      @tailrec def go(tabs: Int, tail: Lst[R]): Lst[R] =
        if(tabs == 0) tail
        else go(tabs - 1, tab :: tail)

      go(line._1, line._2)
    }

    private def fitsOnALine(lineLimit: Int)(implicit measure: R => Int): Boolean =
      foldLeft(0)((n, r) => n + measure(r)) <= lineLimit

    private def flatten: Lst[R] = lst.foldRight(Lst.empty[R])((rt, rs) => rt match {
      case -\/(r) => r :: rs
      case \/-(t) => t.flatten ::: rs
    })
  }
  object Tree {
    def singleton[R](r: R): Tree[R] = Tree(Lst.singleton(-\/(r)))
    def empty[R]: Tree[R] = Tree(Lst.empty)
    def nest[R](tree: Tree[R]): Tree[R] = Tree(Lst.singleton(\/-(tree)))

    implicit def monoid[R]: Monoid[Tree[R]] = new Monoid[Tree[R]] {
      def zero: Tree[R] = empty
      def append(t1: Tree[R], t2: => Tree[R]): Tree[R] = t1 ++ t2
    }

    implicit val foldable: Foldable[Tree] = new Foldable[Tree] {
      def foldMap[A, B](fa: Tree[A])(f: (A) => B)(implicit F: Monoid[B]): B = ???

      def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B = ???

      override def foldLeft[A, B](fa: Tree[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
    }
  }

  implicit def objectOutputInstance[R, Ptr[_]]: ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] =
    new ObjectOutput[FreeObjectOutput[R, Ptr, Unit], R, Ptr] {

      def write(out: FreeObjectOutput[R, Ptr, Unit], r: R): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.write(r).unwrap)

      def writeSubObject[A, B](out: FreeObjectOutput[R, Ptr, Unit], pa: Ptr[A])(f: A => B)(implicit ser: ObjectSerializer[B, R, Ptr]): FreeObjectOutput[R, Ptr, Unit] =
        wrap(out.unwrap >> FreeObjectOutput.writeRec(pa, (a: A) => ser.free(f(a))).unwrap)
    }

  implicit def monadObjectOutputInstance[R, Ptr[_]]: MonadObjectOutput[FreeObjectOutput[R, Ptr, *], R, Ptr] with BindRec[FreeObjectOutput[R, Ptr, *]] =
    new MonadObjectOutput[FreeObjectOutput[R, Ptr, *], R, Ptr] with BindRec[FreeObjectOutput[R, Ptr, *]] {

      def writeRec[A](pa: Ptr[A])(f: A => FreeObjectOutput[R, Ptr, Unit]): FreeObjectOutput[R, Ptr, Unit] =
        FreeObjectOutput.writeRec(pa, f)

      def writer[A](w: R, v: A): FreeObjectOutput[R, Ptr, A] =
        wrap(FreeObjectOutput.write[R, Ptr](w).unwrap >> FreeObjectOutput.point(v).unwrap)

      def write(r: R): FreeObjectOutput[R, Ptr, Unit] =
        FreeObjectOutput.write(r)

      def nest[A](fa: FreeObjectOutput[R, Ptr, A]): FreeObjectOutput[R, Ptr, A] =
        FreeObjectOutput.nest(fa)

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

  implicit def objectSerializerInstance[R, Ptr[_]]: ObjectSerializer[FreeObjectOutput[R, Ptr, Unit], R, Ptr] =
    new ObjectSerializer.FromSerialize[FreeObjectOutput[R, Ptr, Unit], R, Ptr] {
      def serialize[M[_]](a: FreeObjectOutput[R, Ptr, Unit])(implicit ev: MonadObjectOutput[M, R, Ptr]): M[Unit] =
        a.serialize[M]
    }

  implicit def monoidInstance[R, Ptr[_]]: Monoid[FreeObjectOutput[R, Ptr, Unit]] = new Monoid[FreeObjectOutput[R, Ptr, Unit]] {
    def append(f1: FreeObjectOutput[R, Ptr, Unit], f2: => FreeObjectOutput[R, Ptr, Unit]): FreeObjectOutput[R, Ptr, Unit] = f1 ++ f2
    def zero: FreeObjectOutput[R, Ptr, Unit] = empty[R, Ptr]
  }

  implicit def aggregatorInstance[R, Ptr[_]]: Aggregator[FreeObjectOutput[R, Ptr, Unit], R] =
    _ :+ _
}