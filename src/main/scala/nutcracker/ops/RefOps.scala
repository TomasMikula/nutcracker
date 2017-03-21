package nutcracker.ops

import scala.language.{higherKinds, implicitConversions}
import nutcracker.{BranchingPropagation, Dom, Final, JoinDom, Propagation, RelativelyComplementedDom, TriggerF, Unchanged, Updated}
import nutcracker.Trigger.continually
import nutcracker.lib.bool.Bool
import nutcracker.util.ContU

import scalaz.Id.Id
import scalaz.{Applicative, Apply, Bind, Functor, Traverse, ~>}
import scalaz.syntax.bind._

final case class RefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit val dom: Dom.Aux[D, U, Δ]) {
  def observe[M[_]](implicit P: Propagation[M, Ref]) = P.observe(ref)

  def peekC[M[_]](implicit P: Propagation[M, Ref]): ContU[M, D] =
    ContU(f => P.peek(ref)(f))
}

trait ToRefOps {
  implicit def toRefOps[Ref[_], D](ref: Ref[D])(implicit dom: Dom[D]): RefOps[Ref, D, dom.Update, dom.Delta] =
    RefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}

final case class FinalRefOps[Ref[_], D, U, Δ, A](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ], fin: Final.Aux[D, A]) {
  import TriggerF._

  def whenFinal[M[_]](f: A => M[Unit])(implicit M: Propagation[M, Ref]): M[Unit] =
    M.observe(ref).by(λ[Id ~> λ[α => (D => TriggerF[M, α])]](α => d => fin.extract(d) match {
      case Some(a) => Fire(f(a))
      case None => Sleep(α)
    }))

  def whenFinal0[M[_]](f: D => M[Unit])(implicit M: Propagation[M, Ref]): M[Unit] =
    M.observe(ref).by(λ[Id ~> λ[α => (D => TriggerF[M, α])]](α => d =>
      if(fin.isFinal(d)) Fire(f(d))
      else Sleep(α)
    ))

  def >>=[M[_]](f: A => M[Unit])(implicit P: Propagation[M, Ref]): M[Unit] =
    whenFinal(f)

  def asCont[M[_]](implicit P: Propagation[M, Ref]): ContU[M, A] =
    ContU { whenFinal(_) }
}

trait ToFinalRefOps {
  implicit def toFinalRefOps[Ref[_], D](ref: Ref[D])(implicit dom: Dom[D], fin: Final[D]): FinalRefOps[Ref, D, dom.Update, dom.Delta, fin.Out] =
    FinalRefOps[Ref, D, dom.Update, dom.Delta, fin.Out](ref)(dom, fin)
}

final case class JoinRefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit dom: JoinDom.Aux[D, U, Δ]) {

  def set[M[_], A](a: A)(implicit fin: Final.Aux[D, A], P: Propagation[M, Ref]): M[Unit] =
    P.update(ref).by(dom.toJoinUpdate(fin.embed(a)))

  def ==>[M[_]](target: Ref[D])(implicit P: Propagation[M, Ref], M: Functor[M]): M[Unit] =
    P.observe(ref).by(continually[M, D, Δ]((d: D) => P.update(target).by(dom.toJoinUpdate(d))))

  def <==[M[_]](source: Ref[D])(implicit P: Propagation[M, Ref], M: Functor[M]): M[Unit] =
    JoinRefOps(source) ==> ref

  def <=>[M[_]: Apply](target: Ref[D])(implicit P: Propagation[M, Ref]): M[Unit] =
    (this ==> target) *> (this <== target)
}

trait ToJoinRefOps {
  implicit def toJoinRefOps[Ref[_], D](ref: Ref[D])(implicit dom: JoinDom[D]): JoinRefOps[Ref, D, dom.Update, dom.Delta] =
    JoinRefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}

final case class RelativelyComplementedRefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit dom: RelativelyComplementedDom.Aux[D, U, Δ]) {
  import RelativelyComplementedRefOps._

  def exclude[M[_]](d: D)(implicit P: Propagation[M, Ref]): M[Unit] =
    excludeVal(ref, d)

  def remove[M[_], A](a: A)(implicit fin: Final.Aux[D, A], P: Propagation[M, Ref]): M[Unit] =
    exclude(fin.embed(a))

  def excludeThat[M[_]](that: Ref[D])(implicit fin: Final[D], P: Propagation[M, Ref]): M[Unit] =
    excludeRef(ref, that)

  def excludeFrom[M[_]](that: Ref[D])(implicit fin: Final[D], P: Propagation[M, Ref]): M[Unit] =
    excludeRef(that, ref)

  /** Ensure that the two cells resolve to different values. */
  def =!=[M[_]](that: Ref[D])(implicit fin: Final[D], P: Propagation[M, Ref], M: Apply[M]): M[Unit] =
    makeDifferent(ref, that)

  def isDifferentFrom[M[_]](that: Ref[D])(implicit
    fin: Final[D],
    P: BranchingPropagation[M, Ref],
    M: Bind[M]
  ): M[Ref[Bool]] = {
    import P._
    for {
      res <- P.newVar[Bool]
      _ <- res.whenFinal(r => if (r) ref =!= that else ref <=> that)
      _ <- ref.whenFinal0(r1 => that.whenFinal0(r2 => {
        val r = dom.ljoin(r1, r2) match {
          case Unchanged() => false
          case Updated(x, _) => dom.isFailed(x)
        }
        res.set(r)
      }))
    } yield res
  }
}

object RelativelyComplementedRefOps {
  def excludeVal[M[_], Ref[_], D](ref: Ref[D], d: D)(implicit dom: RelativelyComplementedDom[D], P: Propagation[M, Ref]): M[Unit] =
    P.update(ref).by(dom.toComplementUpdate(d))

  def excludeRef[M[_], Ref[_], D](ref1: Ref[D], ref2: Ref[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D], P: Propagation[M, Ref]): M[Unit] =
    ref2.whenFinal0(d => excludeVal(ref1, d))

  def makeDifferent[M[_], Ref[_], D](ref1: Ref[D], ref2: Ref[D])(implicit dom: RelativelyComplementedDom[D], fin: Final[D], P: Propagation[M, Ref], M: Apply[M]): M[Unit] =
    excludeRef(ref1, ref2) *> excludeRef(ref2, ref1)
}

trait ToRelativelyComplementedRefOps {
  implicit def toRelativelyComplementedRefOps[Ref[_], D](ref: Ref[D])(implicit dom: RelativelyComplementedDom[D]): RelativelyComplementedRefOps[Ref, D, dom.Update, dom.Delta] =
    RelativelyComplementedRefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}

final case class RelativelyComplementedRefSeqOps[Ref[_], D, U, Δ](refs: IndexedSeq[Ref[D]])(implicit dom: RelativelyComplementedDom.Aux[D, U, Δ]) {

  private implicit val traverseIterable: Traverse[Iterable] = new Traverse[Iterable] {
    def traverseImpl[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] = {
      fa.foldRight[G[List[B]]](G.point(Nil))((a, gbs) => G.apply2(f(a), gbs)(_ :: _)).map[Iterable[B]](bs => bs)
    }
  }

  def allDifferent[M[_]](implicit
    fin: Final[D],
    P: BranchingPropagation[M, Ref],
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