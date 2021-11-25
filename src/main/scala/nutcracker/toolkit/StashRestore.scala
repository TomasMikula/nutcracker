package nutcracker.toolkit

import nutcracker.util.APairK.Pair
import scalaz.NonEmptyList

trait StashRestore[S] {
  def stash(s: S): S
  def restore(s: S): S
}

object StashRestore {
  implicit def nelInstance[A]: StashRestore[NonEmptyList[A]] = new StashRestore[NonEmptyList[A]] {
    def stash(s: NonEmptyList[A]) = s.head <:: s
    def restore(s: NonEmptyList[A]) = s.tail.toNel.getOrElse { throw new IllegalStateException("Bottom of the stack, nothing to restore.") }
  }

  implicit def kPairInstance[F[_[_]], G[_[_]], A[_]](implicit F: StashRestore[F[A]], G: StashRestore[G[A]]): StashRestore[Pair[F, G, A]] =
    new StashRestore[Pair[F, G, A]] {
      def stash(s: Pair[F, G, A]) = Pair(F.stash(s._1), G.stash(s._2))
      def restore(s: Pair[F, G, A]) = Pair(F.restore(s._1), G.restore(s._2))
    }

  implicit class StashRestoreOps[G[_[_]], A[_]](G: StashRestore[G[A]]) {
    def :*:[F[_[_]]](F: StashRestore[F[A]]): StashRestore[Pair[F, G, A]] =
      kPairInstance[F, G, A](F, G)
  }
}