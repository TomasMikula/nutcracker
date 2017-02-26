package nutcracker

import nutcracker.util.{FreeK, HEqualK, ShowK}

trait Bundle {
  type Lang[K[_], A]
  type State[K[_]]

  type Prg[A] = FreeK[Lang, A]

  def empty[K[_]]: State[K]
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A)
}

trait RefBundle extends Bundle {
  type Ref[_]

  implicit def refEquality: HEqualK[Ref]
  implicit def refShow: ShowK[Ref]

  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A
}