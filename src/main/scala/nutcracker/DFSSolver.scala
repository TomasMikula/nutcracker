package nutcracker

import scala.language.higherKinds
import nutcracker.Assessment._
import nutcracker.util.idToM

import scalaz.{-\/, BindRec, Monad, StateT, StreamT, \/, \/-, ~>}
import scalaz.syntax.monad._

class DFSSolver[Prg[_], St[_[_]], M[_], P[_]](
  interpreter: Prg ~> StateT[M, St[Prg], ?],
  initialState: St[Prg],
  assess: St[Prg] => Assessment[List[Prg[Unit]]],
  fetch: P ~> (St[Prg] => ?)
) {

  type S = St[Prg]

  def solutions[A](p: Prg[P[A]])(implicit M: Monad[M]): StreamT[M, A] = {
    StreamT.wrapEffect(init(p) map { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions_(s) map fetch
    })
  }

  def solutions1[A](p: Prg[P[A]])(implicit M0: Monad[M], M1: BindRec[M]): StreamT[M, (Option[A], Int)] = {
    StreamT.wrapEffect(M0.map(init(p)) { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions1_(s).map({ case (so, n) => (so map {fetch(_)}, n) })(M0)
    })(M0)
  }

  def allSolutions1[A](p: Prg[P[A]])(implicit M0: Monad[M], M1: BindRec[M]): M[(List[A], Int)] = {
    solutions1(p).foldLeftRec((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }


  private def init[A](p: Prg[A])(implicit M: Monad[M]): M[(S, A)] = {
    interpreter(p)(initialState)
  }

  private def solutions_(s: S)(implicit M: Monad[M]): StreamT[M, S] =
    solutions[S](s, StreamT.empty[M, S], (_: S) :: StreamT.empty[M, S])

  private def solutions1_(ps: S)(implicit M0: Monad[M], M1: BindRec[M]): StreamT[M, (Option[S], Int)] = {
    val leafs = solutions[Option[S]](ps, None :: StreamT.empty[M, Option[S]], Some(_: S) :: StreamT.empty[M, Option[S]])

    def takeOne(s: StreamT[M, Option[S]]): M[Option[((Option[S], Int), StreamT[M, Option[S]])]] = {
      def go(sn: (StreamT[M, Option[S]], Int)): M[(StreamT[M, Option[S]], Int) \/ Option[((Option[S], Int), StreamT[M, Option[S]])]] = {
        val (s, n) = sn
        M0.map(s.unconsRec)((o: Option[(Option[S], StreamT[M, Option[S]])]) =>
          o match {
            case Some((Some(a), tail: StreamT[M, Option[S]])) => \/-(Option(((Option(a), n), tail)))
            case Some((None, tail: StreamT[M, Option[S]])) => -\/((tail, n + 1))
            case None =>
              if (n > 0) \/-(Option(((Option.empty[S], n), StreamT.empty[M, Option[S]])))
              else \/-(Option.empty)
          })
      }
      M1.tailrecM((s, 0))(go)
    }

    StreamT.unfoldM[M, (Option[S], Int), StreamT[M, Option[S]]](leafs)(takeOne(_))(M0)
  }

  private def solutions[B](
    s: S,
    failed: StreamT[M, B],
    done: S => StreamT[M, B]
  )(implicit
    M: Monad[M]
  ): StreamT[M, B] =
    assess(s) match {
      case Failed => failed
      case Stuck => failed // TODO: Don't treat as failed.
      case Done => done(s)
      case Incomplete(branches) =>
        StreamT.fromIterable(branches).trans(idToM) flatMap { k =>
          StreamT.wrapEffect[M, B](interpreter(k)(s) map { (su: (S, Unit)) => solutions(su._1, failed, done) })
        }
    }
}
