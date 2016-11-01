import nutcracker.util.{ContU, Inject}
import scala.language.{implicitConversions, higherKinds}
import scalaz.Bind
import scalaz.syntax.bind._

package nutcracker {

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice meet with the given value.
    */
  final case class Meet[+D](value: D) extends AnyVal

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice join with the given value.
    */
  final case class Join[D](value: D) extends AnyVal

  /** When used as a monotonic update, represents the operation of relative complement.
    * When used as a delta, represents the part that was removed.
    * An operation `Diff(d)` applied to value `d0` will result in the new value being
    * `d0 \ d` and the published delta being `Diff(d0 ∧ d)`.
    */
  final case class Diff[+D](value: D) extends AnyVal


  final case class RefOps[Ref[_], D, U, Δ](ref: Ref[D])(implicit dom: Dom.Aux[D, U, Δ]) {

    def ==>[M[_]](target: Ref[D])(implicit inj: Inject[Join[D], U], P: Propagation[M, Ref]): M[Unit] =
      P.valTrigger(ref){ d => FireReload(FinalVars[M, Ref].join(target)(d)) }

    def <=>[M[_]: Bind](target: Ref[D])(implicit inj: Inject[Join[D], U], P: Propagation[M, Ref]): M[Unit] =
      (ref ==> target) >> (target ==> ref)

    def >>=[M[_], A](f: A => M[Unit])(implicit fin: Final.Aux[D, A], P: Propagation[M, Ref]): M[Unit] =
      FinalVars[M, Ref].whenFinal(ref).exec(f)

    def asCont[M[_]](implicit fin: Final[D], P: Propagation[M, Ref]): ContU[M, fin.Out] =
      ContU { FinalVars[M, Ref].whenFinal(ref).exec(_) }

    def peekC[M[_]](implicit P: Propagation[M, Ref]): ContU[M, D] =
      ContU(f => P.peek(ref)(f))

  }

}

package object nutcracker {


  implicit def refOps[Ref[_], D](ref: Ref[D])(implicit dom: Dom[D]) = RefOps[Ref, D, dom.Update, dom.Delta](ref)(dom)
}