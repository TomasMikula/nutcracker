package nutcracker

import scalaz.Id.Id
import scalaz.{Applicative, Bind, ContT}
import scalaz.syntax.monad._

package object util {
  import ops._

  // workaround for https://issues.scala-lang.org/browse/SI-9453
  // suggested by Miles Sabin in the comments
  type Uninhabited = Nothing { type T = Unit }

  type ∃[F[_]] = F[A] forSome { type A }

  type ≈>[F[_[_]], G[_[_]]] = `FunctionK{(* -> *) -> *}`[F, G]
  type ≈~>[F[_[_], _], G[_[_], _]] = `FunctionK{(* -> *) -> * -> *}`[F, G]

  /** Continuation monad with result type `M[Unit]`. */
  type ContU[M[_], A] = ContT[M, Unit, A]
  object ContU {
    def apply[F[_], A](f: (A => F[Unit]) => F[Unit]): ContU[F, A] =
      ContT(f)
    def noop[F[_]: Applicative, A]: ContU[F, A] =
      ContU(k => ().point[F])
    def point[F[_], A](a: A): ContU[F, A] =
      ContU(k => k(a))
    def liftM[F[_]: Bind, A](fa: F[A]): ContU[F, A] =
      ContU(k => fa.flatMap(k))
    def wrapEffect[F[_]: Bind, A](a: F[ContU[F, A]]): ContU[F, A] =
      ContU[F, A](f => a >>= { k => k(f) })
    def absorbEffect[F[_]: Bind, A](a: ContU[F, F[A]]): ContU[F, A] =
      a.flatMap(liftM(_))
    def sequence[F[_]: Applicative, A](cs: ContU[F, A]*): ContU[F, A] =
      sequence(cs)
    def sequence[F[_]: Applicative, A](cs: Iterable[ContU[F, A]]): ContU[F, A] =
      ContU(f => cs.foldRight(List[F[Unit]]())((c, fus) => c(f) :: fus).sequence_)
    def filter[F[_]: Applicative, A](c: ContU[F, A])(p: A => Boolean): ContU[F, A] =
      ContU(f => c(a => if(p(a)) f(a) else ().point[F]))
    def filterMap[F[_]: Applicative, A, B](c: ContU[F, A])(f: A => Option[B]): ContU[F, B] =
      ContU(k => c(a => f(a).fold[F[Unit]](().point[F])(k(_))))

    def tuple2[F[_], A1, A2](c1: ContU[F, A1], c2: ContU[F, A2]): ContU[F, (A1, A2)] =
      for { a1 <- c1; a2 <- c2 } yield (a1, a2)
    def tuple3[F[_], A1, A2, A3](c1: ContU[F, A1], c2: ContU[F, A2], c3: ContU[F, A3]): ContU[F, (A1, A2, A3)] =
      for { a1 <- c1; a2 <- c2; a3 <- c3 } yield (a1, a2, a3)
    def tuple4[F[_], A1, A2, A3, A4](c1: ContU[F, A1], c2: ContU[F, A2], c3: ContU[F, A3], c4: ContU[F, A4]): ContU[F, (A1, A2, A3, A4)] =
      for { a1 <- c1; a2 <- c2; a3 <- c3; a4 <- c4 } yield (a1, a2, a3, a4)
    def tuple5[F[_], A1, A2, A3, A4, A5](c1: ContU[F, A1], c2: ContU[F, A2], c3: ContU[F, A3], c4: ContU[F, A4], c5: ContU[F, A5]): ContU[F, (A1, A2, A3, A4, A5)] =
      for { a1 <- c1; a2 <- c2; a3 <- c3; a4 <- c4; a5 <- c5 } yield (a1, a2, a3, a4, a5)
    def tuple6[F[_], A1, A2, A3, A4, A5, A6](c1: ContU[F, A1], c2: ContU[F, A2], c3: ContU[F, A3], c4: ContU[F, A4], c5: ContU[F, A5], c6: ContU[F, A6]): ContU[F, (A1, A2, A3, A4, A5, A6)] =
      for { a1 <- c1; a2 <- c2; a3 <- c3; a4 <- c4; a5 <- c5; a6 <- c6 } yield (a1, a2, a3, a4, a5, a6)

    implicit class WrappedContU[F[_], A](self: F[ContU[F, A]]) {
      def wrapEffect(implicit F: Bind[F]): ContU[F, A] = ContU.wrapEffect(self)
    }
  }

  type Index[K, V] = TransformedIndex[K, V, V]

  object Index {
    def empty[K, V](f: V => Seq[K]): Index[K, V] =
      TransformedIndex.empty(f, (v, k) => v)
  }

  type WriterState[W, S, A] = WriterStateT[W, S, Id, A]
  object WriterState {
    def apply[W, S, A](run: S => (W, S, A)): WriterState[W, S, A] =
      WriterStateT[W, S, Id, A](run)
  }

  type Desc[Ptr[_]] = FreeObjectOutput[String, Ptr, Unit]

  type DeepShow[A, Ptr[_]] = ObjectSerializer[A, String, Ptr]
}
