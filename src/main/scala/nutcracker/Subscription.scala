package nutcracker

import nutcracker.util.Lst

final class Subscription[K[_]] private (val unsubscribe: Lst[K[Unit]]) extends AnyVal {
  def and(that: Subscription[K]): Subscription[K] = new Subscription(this.unsubscribe ++ that.unsubscribe)
}

object Subscription {
  def apply[K[_]](unsubscribe: K[Unit]): Subscription[K] = new Subscription(Lst.singleton(unsubscribe))
  def apply[K[_]](): Subscription[K] = new Subscription(Lst.empty)
}
