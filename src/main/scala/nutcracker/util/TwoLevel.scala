package nutcracker.util

import scalaz.{Monoid, Plus, PlusEmpty}

case class TwoLevel[F[_], A](_1: F[A], _2: F[A]) {
  def plus(that: TwoLevel[F, A])(implicit F: Plus[F]): TwoLevel[F, A] =
    TwoLevel(F.plus(this._1, that._1), F.plus(this._2, that._2))
}

object TwoLevel extends TwoLevelInstances {
  def empty[F[_], A](implicit F: PlusEmpty[F]): TwoLevel[F, A] =
    TwoLevel(F.empty, F.empty)

  def lift1[F[_], A](fa: F[A])(implicit F: PlusEmpty[F]): TwoLevel[F, A] =
    TwoLevel(fa, F.empty)

  def lift2[F[_], A](fa: F[A])(implicit F: PlusEmpty[F]): TwoLevel[F, A] =
    TwoLevel(F.empty, fa)
}

trait TwoLevelInstances extends TwoLevelInstances1 {
  implicit def catenableInstance[F[_]](implicit F: Catenable[F]): Catenable[TwoLevel[F, *]] =
    new TwoLevelCatenable[F]

  implicit def stratifiedMonoidAggregator[F[_], A](implicit F: PlusEmpty[F]): StratifiedMonoidAggregator[TwoLevel[F, A], F[A]] =
    new StratifiedMonoidAggregator[TwoLevel[F, A], F[A]] {
      def apply(acc: TwoLevel[F, A], a: F[A], level: Int): TwoLevel[F, A] = level match {
        case 0 => TwoLevel(F.plus(acc._1, a), acc._2)
        case _ => TwoLevel(acc._1, F.plus(acc._2, a))
      }

      def zero: TwoLevel[F, A] = TwoLevel.empty[F, A]

      def append(f1: TwoLevel[F, A], f2: => TwoLevel[F, A]): TwoLevel[F, A] = f1 plus f2

      override def initialize(a: F[A], level: Int): TwoLevel[F, A] = level match {
        case 0 => TwoLevel(a, F.empty)
        case _ => TwoLevel(F.empty, a)
      }
    }
}

trait TwoLevelInstances1 {
  implicit def plusEmptyInstance[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[TwoLevel[F, *]] =
    new TwoLevelPlusEmpty[F]

  implicit def monoidInstance[F[_], A](implicit F: PlusEmpty[F]): Monoid[TwoLevel[F, A]] =
    plusEmptyInstance[F].monoid[A]
}

private[util] class TwoLevelPlusEmpty[F[_]](implicit val F: PlusEmpty[F]) extends PlusEmpty[TwoLevel[F, *]] {
  def empty[A]: TwoLevel[F, A] = TwoLevel.empty[F, A]
  def plus[A](a: TwoLevel[F, A], b: => TwoLevel[F, A]): TwoLevel[F, A] = a plus b
}

private[util] class TwoLevelCatenable[F[_]](override implicit val F: Catenable[F]) extends TwoLevelPlusEmpty[F] with Catenable[TwoLevel[F, *]] {
  def cons[A](a: A, fa: TwoLevel[F, A]): TwoLevel[F, A] =
    TwoLevel(F.cons(a, fa._1), fa._2)

  def singleton[A](a: A): TwoLevel[F, A] =
    TwoLevel(F.singleton(a), F.empty)

  def uncons[A](ta: TwoLevel[F, A]): Option[(A, TwoLevel[F, A])] =
    F.uncons(ta._1) match {
      case Some((a, fa)) => Some((a, TwoLevel(fa, ta._2)))
      case None => F.uncons(ta._2) match {
        case Some((a, fa)) => Some((a, TwoLevel(ta._1, fa)))
        case None => None
      }
    }
}