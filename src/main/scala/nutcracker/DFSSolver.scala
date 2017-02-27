package nutcracker

import scala.language.higherKinds
import scalaz.{-\/, BindRec, MonadTell, StreamT, \/, \/-}
import scalaz.syntax.monad._

class DFSSolver[Prg, St, M[_], A](
  interpret: (Prg, St) => M[St],
  assess: St => List[Prg] \/ A
)(implicit ev: StashRestore[St]) {
  type State = (St, List[List[Prg]])

  def init(st: St): State \/ A = assess(st) match {
    case -\/(prgs) => -\/((ev.stash(st), List(prgs)))
    case \/-(a)    => \/-(a)
  }

  def next(s: State)(implicit M0: BindRec[M], M1: MonadTell[M, Int]): M[Option[(A, State)]] =
    M0.tailrecM(s)(step)

  /** Also outputs the number of backtracks. */
  def solutions(st: St)(implicit M0: BindRec[M], M1: MonadTell[M, Int]): StreamT[M, A] =
    init(st) match {
      case -\/(s) => StreamT.unfoldM(s)(next)(M0)
      case \/-(a) => a :: StreamT.empty
    }

  private def step(s: State)(implicit M: MonadTell[M, Int]): M[State \/ Option[(A, State)]] = {
    val (st, stack) = s
    stack match {
      case Nil => M.point(\/-(None))
      case prgs :: tail => prgs match {
        case Nil => M.point(-\/((ev.restore(st), tail)))
        case p :: Nil => interpret(p, st) flatMap { st1 =>
          assess(st1) match {
            case -\/(bs) => M.point(-\/((st1, bs :: tail)))
            case \/-(a) => M.writer(-1, \/-(Some((a, (st1, Nil :: tail)))))
          }
        }
        case p :: ps => M.writer(1, -\/((ev.stash(st), List(p) :: ps :: tail)))
      }
    }
  }
}