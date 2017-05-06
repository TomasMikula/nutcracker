package nutcracker.util

import scalaz.Lens

final case class Uncons[K[_], S](run: S => Option[(S, Lst[K[Unit]])]) extends AnyVal { self =>
  def apply(s: S): Option[(S, Lst[K[Unit]])] = run(s)

  def zoomOut[T](implicit f: Lens[T, S]): Uncons[K, T] =
    Uncons[K, T](t => self(f.get(t)).map({ case (s, ks) => (f.set(t, s), ks) }))

  def orElse(that: Uncons[K, S]): Uncons[K, S] =
    Uncons[K, S](s => self(s).orElse(that(s)))
}