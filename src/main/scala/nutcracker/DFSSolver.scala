package nutcracker

import scala.language.higherKinds
import nutcracker.Assessment._
import nutcracker.util.free.FreeK

import scalaz.Free.Trampoline
import scalaz.Id._
import scalaz.{Monad, StateT, StreamT, ~>}
import scalaz.syntax.monad._

class DFSSolver[F[_[_], _], St[_[_]], M[_]: Monad, P[_]](
  interpreter: FreeK[F, ?] ~> StateT[M, St[FreeK[F, ?]], ?],
  initialState: St[FreeK[F, ?]],
  assess: St[FreeK[F, ?]] => Assessment[List[FreeK[F, Unit]]],
  fetch: P ~> (St[FreeK[F, ?]] => ?)
) {

  type K[A] = FreeK[F, A]
  type S = St[K]

  def solutions[A](p: K[P[A]]): StreamT[M, A] = {
    StreamT.wrapEffect(init(p) map { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions(s) map fetch
    })
  }

  def solutions1[A](p: K[P[A]]): StreamT[M, (Option[A], Int)] = {
    StreamT.wrapEffect(init(p) map { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions1(s) map { case (so, n) => (so map {fetch(_)}, n) }
    })
  }

  def allSolutions1[A](p: K[P[A]]): M[(List[A], Int)] = {
    solutions1(p).foldLeft((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }


  private def init[A](p: K[A]): M[(S, A)] = {
    interpreter(p)(initialState)
  }

  private def solutions(s: S): StreamT[M, S] =
    solutions[S](s, StreamT.empty[M, S], (_: S) :: StreamT.empty[M, S])

  private def solutions1(ps: S): StreamT[M, (Option[S], Int)] = {
    val leafs = solutions[Option[S]](ps, None :: StreamT.empty[M, Option[S]], Some(_: S) :: StreamT.empty[M, Option[S]])

    def takeOne(s: StreamT[M, Option[S]]): M[Option[((Option[S], Int), StreamT[M, Option[S]])]] = takeOne0(s, 0)

    def takeOne0(s: StreamT[M, Option[S]], acc: Int): M[Option[((Option[S], Int), StreamT[M, Option[S]])]] =
      s.uncons flatMap { (o: Option[(Option[S], StreamT[M, Option[S]])]) =>
        o match {
          case Some((Some(a), tail: StreamT[M, Option[S]])) => Option(((Option(a), acc), tail)).point[M]
          case Some((None, tail: StreamT[M, Option[S]])) => takeOne0(tail, acc + 1)
          case None =>
            if (acc > 0) Option(((Option.empty[S], acc), StreamT.empty[M, Option[S]])).point[M]
            else Option.empty.point[M]
        }
      }

    StreamT.unfoldM[M, (Option[S], Int), StreamT[M, Option[S]]](leafs){ takeOne(_) }
  }

  private def solutions[B](
    s: S,
    failed: StreamT[M, B],
    done: S => StreamT[M, B]
  ): StreamT[M, B] =
    assess(s) match {
      case Failed => failed
      case Stuck => failed // TODO: Don't treat as failed.
      case Done => done(s)
      case Incomplete(branches) =>
        StreamT.fromIterable(branches).trans(IdToM) flatMap { k =>
          StreamT.wrapEffect[M, B](interpreter(k)(s) map { (su: (S, Unit)) => solutions(su._1, failed, done) })
        }
    }

  private object IdToM extends (Id ~> M) {
    def apply[A](a: A): M[A] = a.point[M]
  }
}