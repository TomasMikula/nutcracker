package nutcracker.ops

import scala.language.implicitConversions
import nutcracker.{BranchingPropagation, Dom, Final, JoinDom, ObserveSyntaxHelper, Propagation, RelativelyComplementedDom, Subscription, Unchanged, Updated}
import nutcracker.data.bool.Bool
import nutcracker.util.ContU
import scalaz.{Applicative, Apply, Bind, Functor, IndexedContT, Traverse}
import scalaz.syntax.functor._
import scalaz.syntax.apply0._
import scalaz.syntax.bind0._

final case class ValOps[Val[_], D](ref: Val[D]) extends AnyVal {
  def observe[M[_], Var[_]](implicit P: Propagation[M, Var, Val], dom: Dom[D]): ObserveSyntaxHelper[M, D, dom.Update, dom.Delta, P.Trigger] =
    P.observe(ref)

  def peekC[M[_], Var[_]](implicit P: Propagation[M, Var, Val], dom: Dom[D], M: Functor[M]): ContU[M, D] =
    ContU(f => P.peek_(ref)(f))
}

trait ToValOps {
  implicit def toValOps[M[_], Var[_], Val[_], D](ref: Val[D])(implicit P: Propagation[M, Var, Val]): ValOps[Val, D] =
    ValOps[Val, D](ref)

  implicit def toValOps1[M[_], Var[_], Val[_], D](ref: Var[D])(implicit P: Propagation[M, Var, Val]): ValOps[Val, D] =
    ValOps(P.readOnly(ref))
}

final case class VarOps[Var[_], D](ref: Var[D]) extends AnyVal {
  def asVal[M[_], Val[_]](implicit P: Propagation[M, Var, Val]): Val[D] =
    P.readOnly(ref)
}

trait ToVarOps {
  implicit def toVarOps[M[_], Var[_], Val[_], D](ref: Var[D])(implicit P: Propagation[M, Var, Val]): VarOps[Var, D] =
    VarOps(ref)
}

object VarOps extends ToVarOps

final case class FinalValOps[M[_], Var[_], Val[_], D, U, Δ, A](ref: Val[D])(implicit dom: Dom.Aux[D, U, Δ], fin: Final.Aux[D, A], P: Propagation[M, Var, Val]) {

  def whenFinal(f: A => M[Unit]): M[Subscription[M]] =
    P.observe(ref).threshold(d => fin.extract(d) map f)

  def _whenFinal(f: A => M[_])(implicit M: Functor[M]): M[Subscription[M]] =
    whenFinal(a => M.map(f(a))(_ => ()))

  def whenFinal_(f: A => M[Unit])(implicit M: Functor[M]): M[Unit] =
    whenFinal(f).void

  def whenFinal0(f: D => M[Unit]): M[Subscription[M]] =
    P.observe(ref).threshold(d =>
      if(fin.isFinal(d)) Some(f(d))
      else None
    )

  def _whenFinal0(f: D => M[_])(implicit M: Functor[M]): M[Subscription[M]] =
    whenFinal0(d => M.map(f(d))(_ => ()))

  def whenFinal0_(f: D => M[Unit])(implicit M: Functor[M]): M[Unit] =
    whenFinal0(f).void

  def asCont: IndexedContT[Subscription[M], Unit, M, A] =
    IndexedContT { whenFinal(_) }

  def asCont_(implicit M: Functor[M]): ContU[M, A] =
    ContU { whenFinal_(_) }
}

trait ToFinalValOps {
  implicit def toFinalValOps[M[_], Var[_], Val[_], D](ref: Val[D])(implicit dom: Dom[D], fin: Final[D], P: Propagation[M, Var, Val]): FinalValOps[M, Var, Val, D, dom.Update, dom.Delta, fin.Out] =
    FinalValOps[M, Var, Val, D, dom.Update, dom.Delta, fin.Out](ref)(dom, fin, P)

  implicit def toFinalValOps1[M[_], Var[_], Val[_], D](ref: Var[D])(implicit dom: Dom[D], fin: Final[D], P: Propagation[M, Var, Val]): FinalValOps[M, Var, Val, D, dom.Update, dom.Delta, fin.Out] =
    FinalValOps[M, Var, Val, D, dom.Update, dom.Delta, fin.Out](P.readOnly(ref))(dom, fin, P)
}

object FinalValOps extends ToFinalValOps

final case class JoinValOps[M[_], Var[_], Val[_], D, U, Δ](ref: Val[D])(implicit dom: JoinDom.Aux[D, U, Δ], P: Propagation[M, Var, Val]) {

  def ==>(target: Var[D])(implicit M: Functor[M]): M[Subscription[M]] = {
    import P._
    observe(ref).by(continually[D, Δ]((d: D) => P.update(target).by(dom.toJoinUpdate(d))))
  }
}

trait ToJoinValOps {
  implicit def toJoinValOps[M[_], Var[_], Val[_], D](ref: Val[D])(implicit dom: JoinDom[D], P: Propagation[M, Var, Val]): JoinValOps[M, Var, Val, D, dom.Update, dom.Delta] =
    JoinValOps[M, Var, Val, D, dom.Update, dom.Delta](ref)(dom, P)
}

object JoinValOps extends ToJoinValOps

final case class JoinVarOps[M[_], Var[_], Val[_], D, U, Δ](ref: Var[D])(implicit dom: JoinDom.Aux[D, U, Δ], P: Propagation[M, Var, Val]) {
  import JoinValOps._
  import VarOps._

  def set[A](a: A)(implicit fin: Final.Aux[D, A]): M[Unit] =
    P.update(ref).by(dom.toJoinUpdate(fin.embed(a)))

  def <==(src: Val[D])(implicit M: Functor[M]): M[Subscription[M]] = {
    src ==> ref
  }

  def <=>(target: Var[D])(implicit M: Apply[M]): M[Subscription[M]] = {
    ((ref.asVal ==> target) |@| (this <== target.asVal)) (_ and _)
  }
}

trait ToJoinVarOps {
  implicit def toJoinVarOps[M[_], Var[_], Val[_], D](ref: Var[D])(implicit dom: JoinDom[D], P: Propagation[M, Var, Val]): JoinVarOps[M, Var, Val, D, dom.Update, dom.Delta] =
    JoinVarOps[M, Var, Val, D, dom.Update, dom.Delta](ref)(dom, P)
}

object JoinVarOps extends ToJoinVarOps

final case class RelativelyComplementedRefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit dom: RelativelyComplementedDom.Aux[D, U, Δ]) {
  import FinalValOps._
  import JoinVarOps._
  import RelativelyComplementedRefOps._
  import VarOps._

  def exclude[M[_], Val[_]](d: D)(implicit P: Propagation[M, Ref, Val]): M[Unit] =
    excludeVal(ref, d)

  def remove[M[_], Val[_], A](a: A)(implicit fin: Final.Aux[D, A], P: Propagation[M, Ref, Val]): M[Unit] =
    exclude(fin.embed(a))

  def excludeThat[M[_], Val[_]](that: Ref[D])(implicit fin: Final[D], P: Propagation[M, Ref, Val], M: Functor[M]): M[Unit] =
    excludeRef(ref, that.asVal)

  def excludeFrom[M[_], Var[_]](that: Var[D])(implicit fin: Final[D], P: Propagation[M, Var, Ref], M: Functor[M]): M[Unit] =
    excludeRef(that, ref)

  /** Ensure that the two cells resolve to different values. */
  def =!=[M[_], Val[_]](that: Ref[D])(implicit fin: Final[D], P: Propagation[M, Ref, Val], M: Apply[M]): M[Unit] =
    makeDifferent(ref, that)

  def isDifferentFrom[M[_], Val[_]](that: Ref[D])(implicit
    fin: Final[D],
    P: BranchingPropagation[M, Ref, Val],
    M: Bind[M]
  ): M[Ref[Bool]] = {
    import P._
    for {
      res <- P.newVar[Bool]
      _ <- res.whenFinal(r => if (r) ref =!= that else (ref <=> that).void)
      _ <- ref.whenFinal0_(r1 => that.whenFinal0_(r2 => {
        val r = dom.ljoin(r1, r2) match {
          case Unchanged() => false
          case Updated(x, _) => dom.isFailed(x)
        }
        res.set(r)
      }))
    } yield res
  }
}

object RelativelyComplementedRefOps extends ToRelativelyComplementedRefOps {
  import FinalValOps._
  import VarOps._

  def excludeVal[M[_], Var[_], Val[_], D](ref: Var[D], d: D)(implicit dom: RelativelyComplementedDom[D], P: Propagation[M, Var, Val]): M[Unit] =
    P.update(ref).by(dom.toComplementUpdate(d))

  def excludeRef[M[_], Var[_], Val[_], D](ref1: Var[D], ref2: Val[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D], P: Propagation[M, Var, Val], M: Functor[M]): M[Unit] =
    ref2.whenFinal0_(d => excludeVal(ref1, d))

  def makeDifferent[M[_], Var[_], Val[_], D](ref1: Var[D], ref2: Var[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D], P: Propagation[M, Var, Val], M: Apply[M]): M[Unit] =
    excludeRef(ref1, ref2.asVal) *> excludeRef(ref2, ref1.asVal)
}

trait ToRelativelyComplementedRefOps {
  implicit def toRelativelyComplementedRefOps[Ref[_], D](ref: Ref[D])(implicit dom: RelativelyComplementedDom[D]): RelativelyComplementedRefOps[Ref, D, dom.Update, dom.Delta] =
    RelativelyComplementedRefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}

final case class RelativelyComplementedRefSeqOps[Ref[_], D, U, Δ](refs: IndexedSeq[Ref[D]])(implicit dom: RelativelyComplementedDom.Aux[D, U, Δ]) {
  import FinalValOps._
  import RelativelyComplementedRefOps._

  private implicit val traverseIterable: Traverse[Iterable] = new Traverse[Iterable] {
    def traverseImpl[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] = {
      fa.foldRight[G[List[B]]](G.point(Nil))((a, gbs) => G.apply2(f(a), gbs)(_ :: _)).map[Iterable[B]](bs => bs)
    }
  }

  def allDifferent[M[_], Val[_]](implicit
    fin: Final[D],
    P: BranchingPropagation[M, Ref, Val],
    M: Applicative[M]
  ): M[Unit] = {
    import P._
    val n = refs.size
    val T = Traverse[Iterable]
    (T.traverse_(0 until n){ i => refs(i).whenFinal0(d =>
      (T.traverse_(0 until i){ j => refs(j).exclude(d) }) *>
      (T.traverse_(i + 1 until n){ j => refs(j).exclude(d) })
    ) })
  }
}

trait ToRelativelyComplementedRefSeqOps {
  implicit def toRelativelyComplementedRefSeqOps[Ref[_], D](refs: IndexedSeq[Ref[D]])(implicit dom: RelativelyComplementedDom[D]): RelativelyComplementedRefSeqOps[Ref, D, dom.Update, dom.Delta] =
    RelativelyComplementedRefSeqOps[Ref, D, dom.Update, dom.Delta](refs)(dom)
}
