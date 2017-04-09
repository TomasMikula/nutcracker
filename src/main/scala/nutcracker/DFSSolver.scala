package nutcracker

import scala.language.higherKinds
import scalaz.{-\/, BindRec, MonadTell, StreamT, \/, \/-}
import scalaz.syntax.monad._

class DFSSolver[Prg, S, M[_], A](
  interpret: (Prg, S) => M[S],
  assess: S => List[Prg] \/ A
)(implicit ev: StashRestore[S]) {
  type State = (S, List[List[Prg]])

  def init(s: S): State \/ A = assess(s) match {
    case -\/(prgs) => -\/((ev.stash(s), List(prgs)))
    case \/-(a)    => \/-(a)
  }

  def next(s: State)(implicit M0: BindRec[M], M1: MonadTell[M, Int]): M[Option[(A, State)]] =
    M0.tailrecM(s)(step)

  /** Also outputs the number of backtracks. */
  def solutions(s: S)(implicit M0: BindRec[M], M1: MonadTell[M, Int]): StreamT[M, A] =
    init(s) match {
      case -\/(s) => StreamT.unfoldM(s)(next)(M0)
      case \/-(a) => a :: StreamT.empty
    }

  private def step(st: State)(implicit M: MonadTell[M, Int]): M[State \/ Option[(A, State)]] = {
    val (s, stack) = st
    stack match {
      case Nil => M.point(\/-(None))
      case prgs :: tail => prgs match {
        case Nil => M.point(-\/((ev.restore(s), tail)))
        case p :: Nil => interpret(p, s) flatMap { st1 =>
          assess(st1) match {
            case -\/(bs) => M.point(-\/((st1, bs :: tail)))
            case \/-(a) => M.writer(-1, \/-(Some((a, (st1, Nil :: tail)))))
          }
        }
        case p :: ps => M.writer(1, -\/((ev.stash(s), List(p) :: ps :: tail)))
      }
    }
  }
}