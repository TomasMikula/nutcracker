package nutcracker

import scala.language.higherKinds
import nutcracker.Assessment._
import nutcracker.util.FreeK
import nutcracker.util.idToM

import scalaz.{-\/, BindRec, Monad, StateT, StreamT, \/, \/-, ~>}
import scalaz.syntax.monad._

class DFSSolver[F[_[_], _], St[_[_]], M[_], P[_]](
  interpreter: FreeK[F, ?] ~> StateT[M, St[FreeK[F, ?]], ?],
  initialState: St[FreeK[F, ?]],
  assess: St[FreeK[F, ?]] => Assessment[List[FreeK[F, Unit]]],
  fetch: P ~> (St[FreeK[F, ?]] => ?)
) {

  type K[A] = FreeK[F, A]
  type S = St[K]

  def solutions[A](p: K[P[A]])(implicit M: Monad[M]): StreamT[M, A] = {
    StreamT.wrapEffect(init(p) map { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions(s) map fetch
    })
  }

  def solutions1[A](p: K[P[A]])(implicit M0: Monad[M], M1: BindRec[M]): StreamT[M, (Option[A], Int)] = {
    StreamT.wrapEffect(M0.map(init(p)) { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions1(s).map({ case (so, n) => (so map {fetch(_)}, n) })(M0)
    })(M0)
  }

  def allSolutions1[A](p: K[P[A]])(implicit M0: Monad[M], M1: BindRec[M]): M[(List[A], Int)] = {
    solutions1(p).foldLeftRec((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }


  private def init[A](p: K[A])(implicit M: Monad[M]): M[(S, A)] = {
    interpreter(p)(initialState)
  }

  private def solutions(s: S)(implicit M: Monad[M]): StreamT[M, S] =
    solutions[S](s, StreamT.empty[M, S], (_: S) :: StreamT.empty[M, S])

  private def solutions1(ps: S)(implicit M0: Monad[M], M1: BindRec[M]): StreamT[M, (Option[S], Int)] = {
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
      M1.tailrecM(go)((s, 0))
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