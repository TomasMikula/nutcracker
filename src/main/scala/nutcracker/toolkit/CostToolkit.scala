package nutcracker.toolkit

import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.algebraic.NonDecreasingMonoid
import nutcracker.{Assessment, CostApi, Final}
import scalaz.Id.Id
import scalaz.{-\/, BindRec, Monad, StreamT, \/, \/-}

trait CostToolkit[C] extends Toolkit {
  implicit def costMonoid: NonDecreasingMonoid[C]
  implicit val costApi: CostApi.Aux[Prg, C]

  def getCost(s: State): C

  def assess(s: State): Assessment[List[Prg[Unit]]]

  def solveBfs[A, B](p: Prg[A], f: (A, State) => Option[B]): StreamT[Id, (B, C)] =
    solveBfsM[Id, A, B](p, f)

  def solveBfsM[M[_], A, B](p: Prg[A], f: (A, State) => Option[B])(implicit M: Monad[M]): StreamT[M, (B, C)] = {
    val (s, a) = interpret(p, empty)

    new BFSSolver[Prg[Unit], State, M, C, B](
      (pu, s) => M.point(interpret(pu, s)._1),
      s => assess(s) match {
        case Incomplete(bs) => -\/(bs)
        case Done => f(a, s).fold[List[Prg[Unit]] \/ B](-\/(Nil))(\/-(_))
        case Failed | Stuck => -\/(Nil) // TODO: Don't treat Stuck as failed
      },
      getCost
    ).solutions(s)
  }
}

trait CostRefToolkit[C] extends CostToolkit[C] with RefToolkit {

  def solveBfs[D](p: Prg[Val[D]])(implicit fin: Final[D]): StreamT[Id, (fin.Out, C)] =
    solveBfsM[Id, D](p)

  private def solveBfsM[M[_], D](p: Prg[Val[D]])(implicit fin: Final[D], M0: BindRec[M], M1: Monad[M]): StreamT[M, (fin.Out, C)] =
    solveBfsM[M, Val[D], fin.Out](p, (ref, s) => fetchResult(ref, s))

}