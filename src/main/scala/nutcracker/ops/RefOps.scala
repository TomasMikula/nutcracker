package nutcracker.ops

import scala.language.implicitConversions
import nutcracker.{BranchingPropagation, Dom, Final, JoinDom, Propagation, RelativelyComplementedDom, Subscription, Unchanged, Updated}
import nutcracker.data.bool.Bool
import nutcracker.util.{ContU, IndexedContT}
import scalaz.{Applicative, Apply, Bind, Functor, Traverse}
import scalaz.syntax.functor._
import scalaz.syntax.apply0._
import scalaz.syntax.bind0._

final case class ValOps[Val[_], D](ref: Val[D]) extends AnyVal {
  def observe[M[_], Var[_]](implicit P: Propagation.Aux1[M, Var, Val], dom: Dom[D]): P.ObserveSyntaxHelper[D, dom.Delta] =
    P.observe(ref)

  def peekC[M[_], Var[_]](implicit P: Propagation.Aux1[M, Var, Val], dom: Dom[D], M: Functor[M]): ContU[M, D] =
    ContU(f => P.peek_(ref)(f))
}

trait ToValOps {
  implicit def toValOps[M[_], Var[_], Val[_], D](ref: Val[D])(implicit P: Propagation.Aux1[M, Var, Val]): ValOps[Val, D] =
    ValOps[Val, D](ref)

  implicit def toValOps1[M[_], Var[_], D](ref: Var[D])(implicit P: Propagation.Aux0[M, Var]): ValOps[P.Val, D] =
    ValOps(P.readOnly(ref))
}

final case class VarOps[Var[_], D](ref: Var[D]) extends AnyVal {
  def asVal[M[_]](implicit P: Propagation.Aux0[M, Var]): P.Val[D] =
    P.readOnly(ref)
}

trait ToVarOps {
  implicit def toVarOps[M[_], Var[_], D](ref: Var[D])(implicit P: Propagation.Aux0[M, Var]): VarOps[Var, D] =
    VarOps(ref)
}

object VarOps extends ToVarOps

object FinalValOps {
  extension [M[_], D](using P: Propagation[M])(ref: P.Val[D]) {
    def whenFinal[A](f: A => M[Unit])(implicit dom: Dom[D], fin: Final.Aux[D, A]): M[Subscription[M]] =
      whenFinalImpl(P)(ref)(f)

    def _whenFinal[A, B](f: A => M[B])(implicit dom: Dom[D], fin: Final.Aux[D, A]): M[Subscription[M]] = {
      import P.M
      whenFinalImpl(P)(ref)((a: A) => f(a).void)
    }

    def whenFinal_[A](f: A => M[Unit])(implicit dom: Dom[D], fin: Final.Aux[D, A]): M[Unit] = {
      import P.M
      whenFinalImpl(P)(ref)(f).void
    }

    def whenFinal0(f: D => M[Unit])(implicit dom: Dom[D], fin: Final[D]): M[Subscription[M]] =
      whenFinal0Impl(P)(ref)(f)

    def _whenFinal0[B](f: D => M[B])(implicit dom: Dom[D], fin: Final[D]): M[Subscription[M]] = {
      import P.M
      whenFinal0Impl(P)(ref)(d => f(d).void)
    }

    def whenFinal0_(f: D => M[Unit])(implicit dom: Dom[D], fin: Final[D]): M[Unit] = {
      import P.M
      whenFinal0Impl(P)(ref)(f).void
    }

    def asCont[A](implicit dom: Dom[D], fin: Final.Aux[D, A]): IndexedContT[Subscription[M], Unit, M, A] =
      IndexedContT { whenFinalImpl(P)(ref)(_) }

    def asCont_[A](implicit dom: Dom[D], fin: Final.Aux[D, A]): ContU[M, A] = {
      import P.M
      ContU { whenFinalImpl(P)(ref)(_).void }
    }
  }

  private def whenFinalImpl[M[_], D, A](P: Propagation[M])(ref: P.Val[D])(f: A => M[Unit])(implicit dom: Dom[D], fin: Final.Aux[D, A]): M[Subscription[M]] =
    P.observe(ref).threshold(d => fin.extract(d) map f)

  private def whenFinal0Impl[M[_], D](P: Propagation[M])(ref: P.Val[D])(f: D => M[Unit])(implicit dom: Dom[D], fin: Final[D]): M[Subscription[M]] =
    P.observe(ref).threshold(d =>
      if(fin.isFinal(d)) Some(f(d))
      else None
    )
}

trait JoinValOps {
  extension [M[_], D](using P: Propagation[M])(ref: P.Val[D]) {
    def ==>(target: P.Var[D])(implicit dom: JoinDom[D]): M[Subscription[M]] = {
      import P._
      observe(ref).by(continually[D, dom.Delta]((d: D) => P.update(target).by(dom.toJoinUpdate(d))))
    }
  }
}

object JoinValOps extends JoinValOps

trait JoinVarOps {
  import JoinValOps._
  import VarOps._

  extension [M[_], D](using P: Propagation[M])(ref: P.Var[D]) {
    def set[A](a: A)(implicit dom: JoinDom[D], fin: Final.Aux[D, A]): M[Unit] =
      P.update(ref).by(dom.toJoinUpdate(fin.embed(a)))

    def <==(src: P.Val[D])(implicit dom: JoinDom[D]): M[Subscription[M]] = {
      feed(src, ref)
    }

    def <=>(target: P.Var[D])(implicit dom: JoinDom[D]): M[Subscription[M]] = {
      import P.M
      (feed(ref.asVal, target) |@| feed(target.asVal, ref)) (_ and _)
    }
  }

  private def feed[M[_], Var[_], Val[_], D](src: Val[D], tgt: Var[D])(implicit
    P: Propagation.Aux1[M, Var, Val],
    dom: JoinDom[D],
  ): M[Subscription[M]] =
    src ==> tgt
}

object JoinVarOps extends JoinVarOps

object RelativelyComplementedRefOps {
  import FinalValOps._
  import JoinVarOps._
  import VarOps._

  extension [M[_], D](using P: Propagation[M])(ref: P.Var[D]) {
    def exclude(d: D)(implicit dom: RelativelyComplementedDom[D]): M[Unit] =
      excludeVal(ref, d)

    def remove[A](a: A)(implicit dom: RelativelyComplementedDom[D], fin: Final.Aux[D, A]): M[Unit] =
      excludeVal(ref, fin.embed(a))

    def excludeThat(that: P.Var[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D]): M[Unit] =
      excludeRef(ref, that.asVal)

    /** Ensure that the two cells resolve to different values. */
    def =!=(that: P.Var[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D]): M[Unit] =
      makeDifferent(ref, that)
  }

  extension [M[_], D](using P: Propagation[M])(ref: P.Val[D]) {
    def excludeFrom(that: P.Var[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D]): M[Unit] =
      excludeRef(that, ref)
  }

  extension [M[_], D](using P: BranchingPropagation[M])(ref: P.propagation.Var[D]) {
    def isDifferentFrom(that: P.propagation.Var[D])(implicit
      dom: RelativelyComplementedDom[D],
      fin: Final[D],
    ): M[P.propagation.Var[Bool]] = {
      import P._
      for {
        res <- P.newVar[Bool]
        _ <- res.whenFinal((r: Boolean) => if (r) ref =!= that else (ref <=> that).void)
        _ <- ref.whenFinal0_(r1 => that.whenFinal0_(r2 => {
          val r = dom.ljoin(r1, r2).at[Any, Any] match {
            case Unchanged() => false
            case Updated(x, _) => dom.isFailed(x)
          }
          res.set(r)
        }))
      } yield res
    }
  }

  def excludeVal[M[_], Var[_], D](ref: Var[D], d: D)(implicit dom: RelativelyComplementedDom[D], P: Propagation.Aux0[M, Var]): M[Unit] =
    P.update(ref).by(dom.toComplementUpdate(d))

  def excludeRef[M[_], Var[_], Val[_], D](ref1: Var[D], ref2: Val[D])(implicit
    dom: RelativelyComplementedDom[D],
    fin: Final[D],
    P: Propagation.Aux1[M, Var, Val],
  ): M[Unit] =
    ref2.whenFinal0_(d => excludeVal(ref1, d))

  def makeDifferent[M[_], Var[_], D](ref1: Var[D], ref2: Var[D])(implicit
    dom: RelativelyComplementedDom[D],
    fin: Final[D],
    P: Propagation.Aux0[M, Var],
  ): M[Unit] = {
    import P.M
    excludeRef(ref1, ref2.asVal) *> excludeRef(ref2, ref1.asVal)
  }
}

object RelativelyComplementedRefSeqOps {
  import FinalValOps._
  import RelativelyComplementedRefOps._

  private implicit val traverseIterable: Traverse[Iterable] = new Traverse[Iterable] {
    def traverseImpl[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] = {
      fa.foldRight[G[List[B]]](G.point(Nil))((a, gbs) => G.apply2(f(a), gbs)(_ :: _)).map[Iterable[B]](bs => bs)
    }
  }

  extension [M[_], D](using P: BranchingPropagation[M])(refs: IndexedSeq[P.propagation.Var[D]]) {
    def allDifferent(implicit
      dom: RelativelyComplementedDom[D],
      fin: Final[D],
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
}
