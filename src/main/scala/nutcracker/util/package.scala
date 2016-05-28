package nutcracker

import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Cont, Monad, Traverse, |>=|}

package object util {
  type ConstK[A, K[_]] = A

  type ≈>[F[_[_]], G[_[_]]] = FunctionK[F, G]
  type ≈~>[F[_[_], _], G[_[_], _]] = FunctionKA[F, G]
  type ≈>>[F[_[_], _], G[_]] = F ≈~> λ[(K[_], A) => G[A]]

  /** Continuation monad with result type `FreeK[F, Unit]`. */
  type ContF[F[_[_], _], A] = Cont[FreeK[F, Unit], A]
  object ContF {
    def apply[F[_[_], _], A](f: (A => FreeK[F, Unit]) => FreeK[F, Unit]): ContF[F, A] =
      Cont(f)
    def noop[F[_[_], _], A]: ContF[F, A] =
      ContF(k => FreeK.pure(()))
    def sequence[F[_[_], _], A](a: ContF[F, A], b: ContF[F, A]): ContF[F, A] =
      ContF(f => FreeK.sequence_(a(f), b(f)))
  }

  type Index[K, V] = TransformedIndex[K, V, V]

  object Index {
    def empty[K, V](f: V => Seq[K]): Index[K, V] =
      TransformedIndex.empty(f, (v, k) => v)
  }

  type WriterState[W, S, A] = WriterStateT[Id, W, S, A]
  object WriterState {
    def apply[W, S, A](run: S => (W, S, A)): WriterState[W, S, A] =
      WriterStateT[Id, W, S, A](run)
  }

  type StateInterpreter[F[_[_], _]]  = StateInterpreterT[Id, F]
  object StateInterpreter {
    type Aux[F[_[_], _], S[_[_]]] = StateInterpreterT.Aux[Id, F, S]
  }

  type Step[F[_[_], _], S[_[_]]] = StepT[Id, F, S]

  type FreeK[F[_[_], _], A] = FreeKT[F, Id, A]
  object FreeK {

    def pure[F[_[_], _], A](a: A): FreeK[F, A] =
      FreeKT.pure[F, Id, A](a)

    def liftF[F[_[_], _], A](a: F[FreeK[F, ?], A]): FreeK[F, A] =
      FreeKT.liftF[F, Id, A](a)

    def injLiftF[F[_[_], _], G[_[_], _], A](
      a: F[FreeK[G, ?], A])(implicit
      inj: InjectK[F, G]
    ): FreeK[G, A] =
      liftF(inj(a))

    def sequence_[F[_[_], _]](ps: Iterable[FreeK[F, Unit]]): FreeK[F, Unit] =
      ps.foldLeft[FreeK[F, Unit]](FreeK.pure(())) { _ >> _ }

    def sequence_[F[_[_], _]](ps: FreeK[F, Unit]*): FreeK[F, Unit] =
      sequence_(ps)

    def sequence[F[_[_], _], C[_]: Traverse, A](ps: C[FreeK[F, A]]): FreeK[F, C[A]] =
      Traverse[C].sequence[FreeKT[F, Id, ?], A](ps)(FreeKT.freeKTMonad[F, Id])

    def traverse[F[_[_], _], C[_]: Traverse, A, B](ps: C[A])(f: A => FreeK[F, B]): FreeK[F, C[B]] =
      Traverse[C].traverse[FreeK[F, ?], A, B](ps)(f)(FreeKT.freeKTMonad[F, Id])

  }

  implicit def idToM[M[_]](implicit M: Monad[M]): M |>=| Id = new (M |>=| Id) {
    override val MF = implicitly[Monad[Id]]
    override val MG = M

    override def promote[A](a: Id[A]): M[A] = M.point(a)
  }
}
