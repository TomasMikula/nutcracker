package nutcracker.toolkit

import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.{Assessment, BranchingPropagation, Final}
import nutcracker.util.Id
import scalaz.std.anyVal._
import scalaz.{-\/, BindRec, Monad, MonadTell, StreamT, WriterT, \/, \/-}

trait BranchingToolkit extends RefToolkit with StashToolkit {
  implicit val branchingApi: BranchingPropagation.Aux1[Prg, Var, Val]
  implicit def stashRestore: StashRestore[State]

  def assess(s: State): Assessment[List[Prg[Unit]]]

  def solveDfs[A, B](p: Prg[A], f: (A, State) => Option[B]): StreamT[Id, B] =
    solveDfsM0[Id, A, B](p, f)

  def solveDfs[D](p: Prg[Val[D]])(implicit fin: Final[D]): StreamT[scalaz.Id.Id, fin.Out] =
    solveDfsM0[scalaz.Id.Id, D](p)(fin, scalaz.Id.id, scalaz.Id.id)

  def solveDfsAll[D](p: Prg[Val[D]])(implicit fin: Final[D]): List[fin.Out] =
    toList(solveDfs(p))(scalaz.Id.id)

  /** Like [[solveDfs[D]*]], but also outputs the number of times it had to backtrack. */
  def solveDfs1[D](p: Prg[Val[D]])(implicit fin: Final[D]): StreamT[WriterT[Int, Id, *], fin.Out] =
    solveDfsM[WriterT[Int, Id, *], D](p)

  /** Like [[solveDfsAll]], but also returns the number of dead branches explored. */
  def solveDfsAll1[D](p: Prg[Val[D]])(implicit fin: Final[D]): (List[fin.Out], Int) =
    toList[WriterT[Int, Id, *], fin.Out](solveDfs1(p)).run.value.swap

  private implicit val mt: MonadTell[WriterT[Int, Id, *], Int] = WriterT.writerTMonadListen[Int, Id]

  private def solveDfsM[M[_], A, B](p: Prg[A], f: (A, State) => Option[B])(implicit M0: BindRec[M], M1: MonadTell[M, Int]): StreamT[M, B] = {
    val (s, a) = interpret(p, empty)

    new DFSSolver[Prg[Unit], State, M, B](
      (pu, s) => M1.point(interpret(pu, s)._1),
      s => assess(s) match {
        case Incomplete(bs) => -\/(bs)
        case Done => f(a, s).fold[List[Prg[Unit]] \/ B](-\/(Nil))(\/-(_))
        case Failed | Stuck => -\/(Nil)
      }
    ).solutions(s)
  }

  private def solveDfsM0[M[_], A, B](p: Prg[A], f: (A, State) => Option[B])(implicit M0: BindRec[M], M1: Monad[M]): StreamT[M, B] =
    solveDfsM(p, f)(M0, fakeMonadTell(M1))

  private def solveDfsM[M[_], D](p: Prg[Val[D]])(implicit fin: Final[D], M0: BindRec[M], M1: MonadTell[M, Int]): StreamT[M, fin.Out] =
    solveDfsM[M, Val[D], fin.Out](p, (ref, s) => fetchResult(ref, s))

  private def solveDfsM0[M[_], D](p: Prg[Val[D]])(implicit fin: Final[D], M0: BindRec[M], M1: Monad[M]): StreamT[M, fin.Out] =
    solveDfsM(p)(fin, M0, fakeMonadTell(M1))

  private def fakeMonadTell[M[_]](M: Monad[M]): MonadTell[M, Int] = new MonadTell[M, Int] {
    def point[X](a: => X): M[X] = M.point(a)
    def bind[X, Y](fa: M[X])(f: X => M[Y]) = M.bind(fa)(f)
    def writer[X](w: Int, v: X) = point(v)
  }

  private def toList[M[_], A](s: StreamT[M, A])(implicit M0: BindRec[M]): M[List[A]] =
    s.foldLeftRec(List[A]())((as, a) => a :: as)
}

trait FreeBranchingToolkit extends FreeRefToolkit with FreeStashToolkit with BranchingToolkit