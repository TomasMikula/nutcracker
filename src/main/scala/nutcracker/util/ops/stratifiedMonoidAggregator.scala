package nutcracker.util.ops

import nutcracker.util.StratifiedMonoidAggregator
import scala.language.implicitConversions

object stratifiedMonoidAggregator extends ToStratifiedMonoidAggregatorOps

trait ToStratifiedMonoidAggregatorOps {
  implicit def toStratifiedMonoidAggregatorOps[A](a: A): StratifiedMonoidAggregatorOps[A] =
    StratifiedMonoidAggregatorOps(a)
}

final case class StratifiedMonoidAggregatorOps[A](a: A) extends AnyVal {

  def at[W](level: Int)(implicit W: StratifiedMonoidAggregator[W, A]): W =
    W.initialize(a, level)

}