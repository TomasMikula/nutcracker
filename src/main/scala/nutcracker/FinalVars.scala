package nutcracker

import scala.language.higherKinds
import nutcracker.lib.bool._
import nutcracker.lib.bool.Bool._
import nutcracker.ops._
import nutcracker.util.Inject

import scalaz.Id.Id
import scalaz.{Applicative, Bind, Monad, Traverse, ~>}
import scalaz.syntax.bind._

/**
 * Convenience API to work with domains that in the end
 * resolve to a value of a different ("smaller") type,
 * which cannot be refined further without reaching a
 * contradiction (thus is final).
 */
class FinalVars[M[_], Ref[_]](implicit M: Propagation[M, Ref]) {
  import FinalVars._
  import M._

  def variable[A]: VarBuilder[M, Ref, A] = new VarBuilder[M, Ref, A]

  def whenFinal[D](ref: Ref[D])(implicit fin: Final[D], dom: Dom[D]): WhenFinal[M, Ref, D, fin.Out] =
    WhenFinal[M, Ref, D, fin.Out](ref, fin)

  def join[D, U, Δ](ref: Ref[D])(d: D)(implicit inj: Inject[Join[D], U], dom: Dom.Aux[D, U, Δ]): M[Unit] =
    update(ref).by(inj(Join(d)))

  def set[A, D, U, Δ](ref: Ref[D], a: A)(implicit fin: Final.Aux[D, A], dom: Dom.Aux[D, U, Δ], inj: Inject[Join[D], U]): M[Unit] =
    join(ref)(fin.embed(a))

  def remove[D, U, Δ](ref: Ref[D], d: D)(implicit dom: Dom.Aux[D, U, Δ], inj: Inject[Diff[D], U]): M[Unit] =
    update[D](ref).by(inj(Diff(d)))

  def exclude[A, D, U, Δ](ref: Ref[D], a: A)(implicit fin: Final.Aux[D, A], dom: Dom.Aux[D, U, Δ], inj: Inject[Diff[D], U]): M[Unit] =
    remove(ref, fin.embed(a))

  def different[D, U, Δ](d1: Ref[D], d2: Ref[D])(implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    inj: Inject[Diff[D], U],
    M: Bind[M]
  ): M[Unit] = {
    whenFinal(d1).exec0(d => remove(d2, d)) >>
    whenFinal(d2).exec0(d => remove(d1, d))
  }

  private implicit def traverseIterable: Traverse[Iterable] = new Traverse[Iterable] {
    def traverseImpl[G[_], A, B](fa: Iterable[A])(f: A => G[B])(implicit G: Applicative[G]): G[Iterable[B]] = {
      fa.foldRight[G[List[B]]](G.point(Nil))((a, gbs) => G.apply2(f(a), gbs)(_ :: _)).map[Iterable[B]](bs => bs)
    }
  }

  def allDifferent[D, U, Δ](doms: Ref[D]*)(
    implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    inj: Inject[Diff[D], U],
    M: Monad[M]
  ): M[Unit] = {
    val n = doms.size
    val T = Traverse[Iterable]
    (T.traverse_(0 until n){ i => whenFinal(doms(i)).exec0(d =>
      (T.traverse_(0 until i){ j => remove(doms(j), d) }) >>
      (T.traverse_(i + 1 until n){ j => remove(doms(j), d) })
    ) })
  }

  def isDifferent[D, U, Δ](d1: Ref[D], d2: Ref[D])(
    implicit
    dom: Dom.Aux[D, U, Δ],
    fin: Final[D],
    injd: Inject[Diff[D], U],
    injm: Inject[Join[D], U],
    MB: Bind[M]
  ): M[Ref[Bool]] =
    for {
      res <- variable[Boolean]()
      _ <- whenFinal(res).exec(r => if (r) different(d1, d2) else d1 <=> d2)
      _ <- whenFinal(d1).exec0(r1 => whenFinal(d2).exec0(r2 => {
        val r = dom.update(r1, injm(Join(r2))) match {
          case None => false
          case Some((x, _)) => dom.assess(x) == Dom.Failed
        }
        set(res, r)
      }))
    } yield res
}

object FinalVars {
  def apply[M[_], Ref[_]](implicit M: Propagation[M, Ref]): FinalVars[M, Ref] = new FinalVars[M, Ref]

  final class VarBuilder[M[_], Ref[_], A] private[nutcracker](implicit M: Propagation[M, Ref]) {
    def apply[D]()(implicit
      ex: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): M[Ref[D]] = any()
    def any[D]()(implicit
      ex: Final.Aux[D, A],
      dom: DomWithBottom[D]
    ): M[Ref[D]] = M.newCell(dom.bottom)

    def oneOf(as: Set[A]): M[Ref[DecSet[A]]] = M.newCell(DecSet.wrap(as))
    def oneOf(as: A*): M[Ref[DecSet[A]]] = oneOf(as.toSet)

    def count(n: Int): VarsBuilder[M, Ref, A] = new VarsBuilder(n)
  }

  final class VarsBuilder[M[_], Ref[_], A] private[nutcracker](n: Int)(implicit M: Propagation[M, Ref]) {
    def apply[D]()(implicit
      ee: Final.Aux[D, A],
      dom: DomWithBottom[D],
      MA: Applicative[M]
    ): M[Vector[Ref[D]]] = any()
    def any[D]()(implicit
      ee: Final.Aux[D, A],
      dom: DomWithBottom[D],
      MA: Applicative[M]
    ): M[Vector[Ref[D]]] = M.cells(dom.bottom, n)

    def oneOf(as: Set[A])(implicit MA: Applicative[M]): M[Vector[Ref[DecSet[A]]]] = M.cells(DecSet.wrap(as), n)
    def oneOf(as: A*)(implicit MA: Applicative[M]): M[Vector[Ref[DecSet[A]]]] = oneOf(as.toSet)
  }

  final case class WhenFinal[M[_], Ref[_], D, A] private[nutcracker](ref: Ref[D], fin: Final.Aux[D, A])(implicit dom: Dom[D], M: Propagation[M, Ref]) {
    import TriggerF._

    def exec(f: A => M[Unit]): M[Unit] =
      M.observe(ref).by(λ[Id ~> λ[α => (D => TriggerF[M, α])]](α => d => fin.extract(d) match {
        case Some(a) => Fire(f(a))
        case None => Sleep(α)
      }))

    def exec0(f: D => M[Unit]): M[Unit] =
      M.observe(ref).by(λ[Id ~> λ[α => (D => TriggerF[M, α])]](α => d =>
        if(fin.isFinal(d)) Fire(f(d))
        else Sleep(α)
      ))
  }
}